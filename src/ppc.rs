use super::{Addr, Disassembler, DisError, DisResult};

pub enum Uarch {
	Gekko,
	Xenon,
	Ppc206B,
}

#[derive(Debug, PartialEq)]
pub enum Reg {
	Gpr(u8),
	LiteralZero,
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
	Lbzu,
	Lbzux,
	Lbzx,
	Lha,
	Lhau,
	Lhaux,
	Lhax,
	Lhz,
	Lhzu,
	Lhzux,
	Lhzx,
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

fn x_ral(instr: u32) -> Reg {
	let reg = x_ra(instr);

	match reg {
		Reg::Gpr(0) => Reg::LiteralZero,
		_           => reg,
	}
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

fn d_rtdra(mne: Mne, instr: u32) -> Op {
	Op::RtDRa(mne, d_rt(instr), d_d(instr), d_ra(instr))
}

fn x_rtrarb(mne: Mne, instr: u32) -> Op {
	Op::RtRaRb(mne, x_rt(instr), x_ra(instr), x_rb(instr))
}

fn x_rtralrb(mne: Mne, instr: u32) -> Op {
	Op::RtRaRb(mne, x_rt(instr), x_ral(instr), x_rb(instr))
}

#[allow(unused)]
fn decode_special(instr: u32, addr: Addr, uarch: Uarch) -> Result<Op, DisError> {
	let op = match x_xo(instr) {
		87  => x_rtralrb(Mne::Lbzx,  instr),

		119 => x_rtrarb( Mne::Lbzux, instr),

		279 => x_rtralrb(Mne::Lhzx,  instr),

		311 => x_rtrarb( Mne::Lhzux, instr),

		343 => x_rtralrb(Mne::Lhax,  instr),

		375 => x_rtrarb( Mne::Lhaux, instr),

		_ => return Err(DisError::Unknown{num_bytes: 4}),
	};

	Ok(op)
}

#[allow(unused)]
pub fn decode(instr: u32, addr: Addr, uarch: Uarch) -> Result<Op, DisError> {
	let op = match opcd(instr) {
		31 => return decode_special(instr, addr, uarch),

		34 => d_rtdra(Mne::Lbz,  instr),
		35 => d_rtdra(Mne::Lbzu, instr),

		40 => d_rtdra(Mne::Lhz,  instr),
		41 => d_rtdra(Mne::Lhzu, instr),
		42 => d_rtdra(Mne::Lha,  instr),
		43 => d_rtdra(Mne::Lhau, instr),

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
	fn decode_lbzu() {
		assert_eq!(decode_i(0x8D23FFFF), Op::RtDRa(Mne::Lbzu, Reg::Gpr(9), -1, Reg::Gpr(3)));
	}

	#[test]
	fn decode_lbzux() {
		assert_eq!(decode_i(0x7D2348EE), Op::RtRaRb(Mne::Lbzux, Reg::Gpr(9), Reg::Gpr( 3), Reg::Gpr(9)));
		assert_eq!(decode_i(0x7D0A40EE), Op::RtRaRb(Mne::Lbzux, Reg::Gpr(8), Reg::Gpr(10), Reg::Gpr(8)));

		assert_eq!(decode_i(0x7C0000EE), Op::RtRaRb(Mne::Lbzux, Reg::Gpr(0), Reg::Gpr(0), Reg::Gpr(0)));
	}

	#[test]
	fn decode_lbzx() {
		assert_eq!(decode_i(0x7C7F50AE), Op::RtRaRb(Mne::Lbzx, Reg::Gpr( 3), Reg::Gpr(31), Reg::Gpr(10)));
		assert_eq!(decode_i(0x7D4340AE), Op::RtRaRb(Mne::Lbzx, Reg::Gpr(10), Reg::Gpr( 3), Reg::Gpr( 8)));

		assert_eq!(decode_i(0x7C0000AE), Op::RtRaRb(Mne::Lbzx, Reg::Gpr(0), Reg::LiteralZero, Reg::Gpr(0)));
	}

	#[test]
	fn decode_lha() {
		assert_eq!(decode_i(0xA8050000), Op::RtDRa(Mne::Lha, Reg::Gpr(0), 0, Reg::Gpr(5)));
		assert_eq!(decode_i(0xA8E70002), Op::RtDRa(Mne::Lha, Reg::Gpr(7), 2, Reg::Gpr(7)));
	}

	#[test]
	fn decode_lhau() {
		assert_eq!(decode_i(0xACE80002), Op::RtDRa(Mne::Lhau, Reg::Gpr(7), 2, Reg::Gpr(8)));
		assert_eq!(decode_i(0xACC40002), Op::RtDRa(Mne::Lhau, Reg::Gpr(6), 2, Reg::Gpr(4)));
	}

	#[test]
	fn decode_lhaux() {
		assert_eq!(decode_i(0x7C7F52EE), Op::RtRaRb(Mne::Lhaux, Reg::Gpr( 3), Reg::Gpr(31), Reg::Gpr(10)));
		assert_eq!(decode_i(0x7D4342EE), Op::RtRaRb(Mne::Lhaux, Reg::Gpr(10), Reg::Gpr( 3), Reg::Gpr( 8)));

		assert_eq!(decode_i(0x7C0002EE), Op::RtRaRb(Mne::Lhaux, Reg::Gpr(0), Reg::Gpr(0), Reg::Gpr(0)));
	}

	#[test]
	fn decode_lhax() {
		assert_eq!(decode_i(0x7CFB3AAE), Op::RtRaRb(Mne::Lhax, Reg::Gpr( 7), Reg::Gpr(27), Reg::Gpr(7)));
		assert_eq!(decode_i(0x7DDE4AAE), Op::RtRaRb(Mne::Lhax, Reg::Gpr(14), Reg::Gpr(30), Reg::Gpr(9)));

		assert_eq!(decode_i(0x7C0002AE), Op::RtRaRb(Mne::Lhax, Reg::Gpr(0), Reg::LiteralZero, Reg::Gpr(0)));
	}

	#[test]
	fn decode_lhz() {
		assert_eq!(decode_i(0xA09E0008), Op::RtDRa(Mne::Lhz, Reg::Gpr(4),      8, Reg::Gpr(30)));
		assert_eq!(decode_i(0xA0E9B328), Op::RtDRa(Mne::Lhz, Reg::Gpr(7), -19672, Reg::Gpr( 9)));
	}

	#[test]
	fn decode_lhzu() {
		assert_eq!(decode_i(0xA4E80002), Op::RtDRa(Mne::Lhzu, Reg::Gpr(7), 2, Reg::Gpr(8)));
	}

	#[test]
	fn decode_lhzux() {
		assert_eq!(decode_i(0x7D44A26E), Op::RtRaRb(Mne::Lhzux, Reg::Gpr(10), Reg::Gpr(4), Reg::Gpr(20)));

		assert_eq!(decode_i(0x7C00026E), Op::RtRaRb(Mne::Lhzux, Reg::Gpr(0), Reg::Gpr(0), Reg::Gpr(0)));
	}

	#[test]
	fn decode_lhzx() {
		assert_eq!(decode_i(0x7D674A2E), Op::RtRaRb(Mne::Lhzx, Reg::Gpr(11), Reg::Gpr(7), Reg::Gpr(9)));
		assert_eq!(decode_i(0x7CC64A2E), Op::RtRaRb(Mne::Lhzx, Reg::Gpr( 6), Reg::Gpr(6), Reg::Gpr(9)));

		assert_eq!(decode_i(0x7C00022E), Op::RtRaRb(Mne::Lhzx, Reg::Gpr(0), Reg::LiteralZero, Reg::Gpr(0)));
	}
}

