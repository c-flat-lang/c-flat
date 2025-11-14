pub mod emit;

// TODO: Registers are allocated but never freed untill the function ends
// This is fine unless there are more then 32(or whatever the max register count is) variables
