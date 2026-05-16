#!/usr/bin/env python3

import difflib
from enum import Enum, auto
import subprocess
from pathlib import Path

DEFAULT_BUILD_OUTPUT = """
                     Tokenizer
                        Parser
              SemanticAnalyzer
                   TypeChecker
            IR Code Generation
        LocalFunctionVariables
              ControlFlowGraph
                 StructuringIr
              LivenessAnalysis
                          Emit
""".strip()


class Target(Enum):
    x86_64_linux = auto()
    wasm32 = auto()
    bitbeat = auto()

    def __str__(self):
        match self:
            case self.x86_64_linux:
                return "x86_64-linux"
            case self.wasm32:
                return "wasm32"
            case self.bitbeat:
                return "bitbeat"


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
):
    passed = 0
    failed = 0
    updated = 0

    files = sorted(
        f for f in Path(test_dir).iterdir() if f.is_file() and f.suffix != ".output"
    )

    for file in files:
        print("\n========================")
        print("Testing:", file)

        result = subprocess.run(
            command
            + ([debug.as_arg()] if debug != DebugInfoAtStage.Nothing else [])
            + [f"--target={target}", str(file)],
            capture_output=True,
            text=True,
        )

        stderr = result.stderr.rstrip()
        stdout = result.stdout.rstrip()
        program_ouput = stderr + stdout

        snapshot = snapshot_path(file, target, debug)

        if not snapshot.exists():

            print("\nNo snapshot found.")
            print("\nProduced output:\n")
            print(program_ouput)

            if ask_yes_no(f"\nCreate snapshot {snapshot.name}?"):
                snapshot.write_text(program_ouput)
                updated += 1
                print("Snapshot created.")
                continue

            failed += 1
            continue

        expected = snapshot.read_text().rstrip()

        if program_ouput == expected:
            print("PASS")
            passed += 1
            continue

        print("SNAPSHOT DIFFERENCE")

        diff = difflib.unified_diff(
            expected.splitlines(),
            program_ouput.splitlines(),
            fromfile="expected",
            tofile="actual",
            lineterm="",
        )

        print("\n".join(diff))

        if ask_yes_no(f"\nUpdate snapshot {snapshot.name}?"):
            snapshot.write_text(program_ouput)
            updated += 1
            print("Snapshot updated.")
        else:
            failed += 1
            print("FAIL")

    print("\n========================")
    print("Summary")
    print("========================")
    print("Passed :", passed)
    print("Failed :", failed)
    print("Updated:", updated)

    if failed:
        exit(1)


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


def run_test_on_target(target: Target):
    run_snapshot_tests(
        "./testing/test",
        ["./target/debug/cflat"],
        target,
        DebugInfoAtStage.Token,
    )

    run_snapshot_tests(
        "./testing/test",
        ["./target/debug/cflat"],
        target,
        DebugInfoAtStage.Ast,
    )

    run_snapshot_tests(
        "./testing/test",
        ["./target/debug/cflat"],
        target,
        DebugInfoAtStage.IrGenerator,
    )

    run_snapshot_tests(
        "./testing/test",
        ["./target/debug/cflat"],
        target,
        DebugInfoAtStage.Emit,
    )

    run_snapshot_tests(
        "./testing/test",
        ["./target/debug/cflat", "run"],
        target,
        DebugInfoAtStage.Nothing,
    )


if __name__ == "__main__":
    if not compile():
        print("Build failed")
        exit(1)
    run_test_on_target(Target.wasm32)
    run_test_on_target(Target.x86_64_linux)
