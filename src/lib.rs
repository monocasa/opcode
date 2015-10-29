pub mod arm;
pub mod chip8;
pub mod ppc;

pub enum Arch {
	Arm,
	Chip8,
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

pub type DisResult = Result<(String, usize), DisError>;

pub trait Disassembler {
	fn disassemble(&self, Addr, &[u8]) -> DisResult;

	fn bytes_per_unit(&self) -> u16;
	fn typical_num_units(&self) -> u16;
}

