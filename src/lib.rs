pub mod arm;
pub mod chip8;
pub mod ppc;

pub enum Arch {
	Arm,
	PowerPC,
}

pub type Addr = u64;

pub enum DisError {
	Unknown{ num_bytes: usize },
	Unaligned{ desired_alignment: usize },
	MemOverflow,
	AddrOutOfRange{ max_addr: Addr },
}

pub type DisResult = Result<(String, String, usize), DisError>;

pub trait Disassembler {
	fn disassemble(&self, Addr, &[u8]) -> DisResult;
}

