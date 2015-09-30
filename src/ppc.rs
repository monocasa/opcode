use super::{Addr, Disassembler, DisError, DisResult};

pub enum Uarch {
	Gekko,
	Xenon,
	Ppc206B,
}

#[derive(Debug, PartialEq)]
pub enum Reg {
	Gpr(u8),
	Literal0,
	Cr,
	CrBit(u8),
	Lr,
	Ctr,
	Xer,
	VrSave,
	Fpr(u8),
	Fpscr,
	Vr(u8),
	Vscr,
	Spr(u16),
}

#[derive(Debug, PartialEq)]
pub enum Mne {
	Addi,
	Addis,
	Lbz,
	Lbzx,
}

#[derive(Debug, PartialEq)]
pub enum Op {
	RtDRa(Mne, Reg, i16, Reg),
	RtRaRb(Mne, Reg, Reg, Reg),
}

fn opcd(instr: u32) -> u16 {
	((instr >> 26) & 0x3F) as u16
}

fn d_rt(instr: u32) -> Reg {
	Reg::Gpr(((instr >> 21) & 0x1F) as u8)
}

fn d_ra(instr: u32) -> Reg {
	Reg::Gpr(((instr >> 16) & 0x1F) as u8)
}

fn d_d(instr: u32) -> i16 {
	(instr & 0xFFFF) as i16
}

fn x_ra(instr: u32) -> Reg {
	Reg::Gpr(((instr >> 16) & 0x1F) as u8)
}

fn x_rb(instr: u32) -> Reg {
	Reg::Gpr(((instr >> 11) & 0x1F) as u8)
}

fn x_rt(instr: u32) -> Reg {
	Reg::Gpr(((instr >> 21) & 0x1F) as u8)
}

fn x_xo(instr: u32) -> u16 {
	((instr >> 1) & 0x3FF) as u16
}

#[allow(unused)]
fn decode_special(instr: u32, addr: Addr, uarch: Uarch) -> Result<Op, DisError> {
	let op = match x_xo(instr) {
		87 => Op::RtRaRb(Mne::Lbzx, x_rt(instr), x_ra(instr), x_rb(instr)),

		_ => return Err(DisError::Unknown{num_bytes: 4}),
	};

	Ok(op)
}

#[allow(unused)]
pub fn decode(instr: u32, addr: Addr, uarch: Uarch) -> Result<Op, DisError> {
	let op = match opcd(instr) {
		31 => return decode_special(instr, addr, uarch),
		34 => Op::RtDRa(Mne::Lbz, d_rt(instr), d_d(instr), d_ra(instr)),
		_ => return Err(DisError::Unknown{num_bytes: 4}),
	};

	Ok(op)
}

pub struct PpcDis {
	#[allow(dead_code)]
	uarch: Uarch,
}

impl PpcDis {
	pub fn new() -> PpcDis {
		PpcDis {
			uarch: Uarch::Ppc206B,
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

#[cfg(test)]
mod tests {
	use super::*;

	fn decode_i(instr: u32) -> Op {
		decode(instr, 0, Uarch::Ppc206B).unwrap()
	}

	#[test]
	fn decode_lbz() {
		assert_eq!(decode_i(0x89230080), Op::RtDRa(Mne::Lbz, Reg::Gpr(9),    128, Reg::Gpr( 3)));
		assert_eq!(decode_i(0x88EAB004), Op::RtDRa(Mne::Lbz, Reg::Gpr(7), -20476, Reg::Gpr(10)));
	}

	#[test]
	fn decode_lbzx() {
		assert_eq!(decode_i(0x7C7F50AE), Op::RtRaRb(Mne::Lbzx, Reg::Gpr( 3), Reg::Gpr(31), Reg::Gpr(10)));
		assert_eq!(decode_i(0x7D4340AE), Op::RtRaRb(Mne::Lbzx, Reg::Gpr(10), Reg::Gpr( 3), Reg::Gpr( 8)));
	}
}

