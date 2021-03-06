pub mod arm;
pub mod chip8;
pub mod mips;
pub mod ppc;

pub enum Arch {
	Arm,
	Chip8,
	Mips,
	PowerPC,
}

pub type Addr = u64;

#[derive(Debug, PartialEq)]
pub enum DisError {
	Unknown{ num_bytes: usize },
	Unaligned{ desired_alignment: usize },
	AddrOutOfRange{ max_addr: Addr },
	MemOverflow,
}

#[derive(Debug, PartialEq)]
pub enum AddrTarget {
	Absolute(Addr),
	Relative(i64),
	Symbol(String),
}

pub type DisResult = Result<(String, usize, bool), DisError>;

pub trait Disassembler {
	fn disassemble(&self, Addr, &[u8]) -> DisResult;

	fn bytes_per_unit(&self) -> u16;
}

