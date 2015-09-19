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
	fn op_num_bytes_hint(&self) -> u8;
}

