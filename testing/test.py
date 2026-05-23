#!/usr/bin/env python3

import itertools
import functools
import argparse
import difflib
from enum import Enum, auto
import subprocess
from pathlib import Path
from dataclasses import dataclass
import platform


class Target(Enum):
    x86_64_linux = auto()
    wasm32 = auto()
    # We are not testing bitbeat cause it is so far behide
    bitbeat = auto()

    def __str__(self):
        match self:
            case self.x86_64_linux:
                return "x86_64-linux"
            case self.wasm32:
                return "wasm32"
            case self.bitbeat:
                return "bitbeat"

    def can_run(self):
        platform_name = platform.system()
        match self:
            case self.x86_64_linux if platform_name == "Linux":
                return False
            case self.wasm32:  #  doesnt matter can always run
                return True
            case self.bitbeat:  # same
                return True
            case _:
                return False


class DebugInfoAtStage(Enum):
    Nothing = auto()
    Token = auto()
    Ast = auto()
    IrGenerator = auto()
    Emit = auto()

    def as_path(self) -> str:
        match self:
            case self.Nothing:
                return "output"
            case self.Token:
                return "token"
            case self.Ast:
                return "ast"
            case self.IrGenerator:
                return "ir_gen"
            case self.Emit:
                return "emit"

    def as_arg(self) -> str:
        match self:
            case self.Nothing:
                return ""
            case self.Token:
                return "-t"
            case self.Ast:
                return "-a"
            case self.IrGenerator:
                return "-ir"
            case self.Emit:
                return "--dump-after=emit"


@dataclass
class Test:
    filename: str
    output: str
    target: Target


@dataclass
class TestResult:
    passed: list[Test]
    failed: list[Test]
    updated: list[Test]


def ask_yes_no(prompt: str) -> bool:
    while True:
        answer = input(f"{prompt} [y/n]: ").strip().lower()

        if answer in ("y", "yes"):
            return True

        if answer in ("n", "no"):
            return False


def snapshot_path(file: Path, target: Target, debug: DebugInfoAtStage) -> Path:
    """
    foo.cb -> foo.output
    """
    new_path = Path(*("datatest" if part == "test" else part for part in file.parts))
    new_path = Path(
        f"{new_path.parent}/{target}/{new_path.stem}/{new_path.stem}.{debug.as_path()}"
    )

    new_path.parent.mkdir(parents=True, exist_ok=True)

    return new_path


def run_snapshot_tests(
    test_dir: str,
    command: list[str],
    target: Target,
    debug: DebugInfoAtStage,
    quiet: bool,
) -> TestResult:
    passed = []
    failed = []
    updated = []

    files = sorted(
        f for f in Path(test_dir).iterdir() if f.is_file() and f.suffix != ".output"
    )

    for file in files:
        if not quiet:
            print("\n========================")
            print("Testing:", file)

        if not target.can_run() and debug == DebugInfoAtStage.Nothing:
            # Skipped!
            continue

        result = subprocess.run(
            command
            + ([debug.as_arg()] if debug != DebugInfoAtStage.Nothing else [])
            + [f"--target={target}", str(file)],
            capture_output=True,
            text=True,
        )

        stderr = result.stderr.rstrip()
        stdout = result.stdout.rstrip()
        program_output = "\n".join(x for x in [stderr, stdout] if x)

        snapshot = snapshot_path(file, target, debug)

        test = Test(
            filename=file.name,
            output=snapshot.name,
            target=target,
        )

        if quiet and not snapshot.exists():
            failed.append(test)
            continue

        elif not snapshot.exists():
            print("\nNo snapshot found.")
            print("\nProduced output:\n")
            print(program_output)

            if ask_yes_no(f"\nCreate snapshot {snapshot.name}?"):
                snapshot.write_text(program_output)
                updated.append(test)
                print("Snapshot created.")
                continue

            failed.append(test)
            continue

        expected = snapshot.read_text().rstrip()

        if program_output == expected:
            if not quiet:
                print("PASS")
            passed.append(test)
            continue

        if not quiet:
            print("SNAPSHOT DIFFERENCE")

        diff = difflib.unified_diff(
            expected.splitlines(),
            program_output.splitlines(),
            fromfile="expected",
            tofile="actual",
            lineterm="",
        )

        if not quiet:
            print("\n".join(diff))

        if quiet:
            failed.append(test)
        elif ask_yes_no(f"\nUpdate snapshot {snapshot.name}?"):
            snapshot.write_text(program_output)
            updated.append(test)
            print("Snapshot updated.")
        else:
            failed.append(test)
            print("FAIL")

    return TestResult(passed, failed, updated)


def compile():
    project_root = Path(__file__).resolve().parent.parent

    result = subprocess.run(
        [
            "cargo",
            "build",
            "--features",
            "wasm-runtime debug",
        ],
        cwd=project_root,
        capture_output=True,
        text=True,
    )

    stdout = result.stdout.rstrip()
    stderr = result.stderr.rstrip()

    if stdout:
        print(stdout)

    if stderr:
        print(stderr)

    return result.returncode == 0


def run_test_on_target(target: Target, quiet: bool) -> TestResult:
    results = [
        run_snapshot_tests(
            "./testing/test",
            ["./target/debug/cflat"],
            target,
            DebugInfoAtStage.Token,
            quiet,
        ),
        run_snapshot_tests(
            "./testing/test",
            ["./target/debug/cflat"],
            target,
            DebugInfoAtStage.Ast,
            quiet,
        ),
        run_snapshot_tests(
            "./testing/test",
            ["./target/debug/cflat"],
            target,
            DebugInfoAtStage.IrGenerator,
            quiet,
        ),
        run_snapshot_tests(
            "./testing/test",
            ["./target/debug/cflat"],
            target,
            DebugInfoAtStage.Emit,
            quiet,
        ),
        run_snapshot_tests(
            "./testing/test",
            ["./target/debug/cflat", "run"],
            target,
            DebugInfoAtStage.Nothing,
            quiet,
        ),
    ]

    return functools.reduce(flatten_test_resuts, results, TestResult([], [], []))


def flatten_test_resuts(acc: TestResult, r: TestResult):
    acc.passed = list(itertools.chain(acc.passed, r.passed))
    acc.failed = list(itertools.chain(acc.failed, r.failed))
    acc.updated = list(itertools.chain(acc.updated, r.updated))
    return acc


def main():
    parser = argparse.ArgumentParser(description="Run snapshot tests")

    parser.add_argument(
        "-q",
        "--quiet",
        action="store_true",
        help="Suppress output and return status only",
    )

    args = parser.parse_args()

    if not compile():
        if not args.quiet:
            print("Build failed")
        exit(1)
    results = [
        run_test_on_target(Target.wasm32, args.quiet),
        run_test_on_target(Target.x86_64_linux, args.quiet),
    ]

    result = functools.reduce(flatten_test_resuts, results, TestResult([], [], []))

    if not args.quiet:
        print("\n========================")
        print("Summary")
        print("========================")
        for test in result.passed:
            print(f"passed '{test.filename}' -> '{test.output}' {test.target}")
        for test in result.updated:
            print(f"updated '{test.filename}' -> '{test.output}' {test.target}")
        for test in result.failed:
            print(f"failed '{test.filename}' -> '{test.output}' {test.target}")
        print("\n========================")
        print("TLDR Summary")
        print("========================")
        print("Passed: ", len(result.passed))
        print("Updated: ", len(result.updated))
        print("Failed: ", len(result.failed))

    exit(1 if len(result.failed) > 0 else 0)


if __name__ == "__main__":
    main()
