use super::{Addr, Disassembler, DisError, DisResult};

pub enum PpcUarch {
	Gekko,
	Xenon,
	Ppc206B,
}

pub enum PpcReg {
	Gpr{ reg: u8 },
	Literal0,
	Cr,
	CrBit{ bit: u8 },
	Lr,
	Ctr,
	Xer,
	VrSave,
	Fpr{ reg: u8 },
	Fpscr,
	Vr{ reg: u8 },
	Vscr,
	Spr{ reg: u16 },
}

pub enum PpcOperands {
	RtRaSi{ rt: PpcReg, ra: PpcReg, si: i16 },
}

pub enum PpcMnemonic {
	Addi,
	Addis,
}

#[allow(dead_code)]
pub struct PpcInstr {
	mnemonic: PpcMnemonic,
	operands: PpcOperands,
}

pub struct PpcDis {
	#[allow(dead_code)]
	uarch: PpcUarch,
}

impl PpcDis {
	pub fn new() -> PpcDis {
		PpcDis {
			uarch: PpcUarch::Ppc206B,
		}
	}
}

impl Disassembler for PpcDis {
	#[allow(unused_variables)]
	fn disassemble(&self, addr: Addr, buf: &[u8]) -> DisResult  {
		if buf.len() < 4 {
			return Err(DisError::MemOverflow);
		}

		if (addr % 4) != 0 {
			return Err( DisError::Unaligned{ desired_alignment: 4, } );
		}

		Err( DisError::Unknown{ num_bytes: 4 } )
	}

	fn op_num_bytes_hint(&self) -> u8 {
		4
	}
}

