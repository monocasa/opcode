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
	Lwz,
	Lwzu,
	Lwzux,
	Lwzx,
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

fn ra_or_literal_zero(reg: Reg) -> Reg {
	match reg {
		Reg::Gpr(0) => Reg::LiteralZero,
		_           => reg,
	}
}

fn d_rtdra(mne: Mne, instr: u32) -> Op {
	Op::RtDRa(mne, d_rt(instr), d_d(instr), d_ra(instr))
}

fn d_rtdral(mne: Mne, instr: u32) -> Op {
	Op::RtDRa(mne, d_rt(instr), d_d(instr), ra_or_literal_zero(d_ra(instr)))
}

fn x_rtrarb(mne: Mne, instr: u32) -> Op {
	Op::RtRaRb(mne, x_rt(instr), x_ra(instr), x_rb(instr))
}

fn x_rtralrb(mne: Mne, instr: u32) -> Op {
	Op::RtRaRb(mne, x_rt(instr), ra_or_literal_zero(x_ra(instr)), x_rb(instr))
}

#[allow(unused)]
fn decode_special(instr: u32, addr: Addr, uarch: Uarch) -> Result<Op, DisError> {
	let op = match x_xo(instr) {
		23  => x_rtralrb(Mne::Lwzx,  instr),

		55  => x_rtrarb( Mne::Lwzux, instr),

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

		32 => d_rtdral(Mne::Lwz, instr),
		33 => d_rtdra(Mne::Lwzu, instr),
		34 => d_rtdral(Mne::Lbz, instr),
		35 => d_rtdra(Mne::Lbzu, instr),

		40 => d_rtdral(Mne::Lhz, instr),
		41 => d_rtdra(Mne::Lhzu, instr),
		42 => d_rtdral(Mne::Lha, instr),
		43 => d_rtdra(Mne::Lhau, instr),

		_ => return Err(DisError::Unknown{num_bytes: 4}),
	};

	Ok(op)
}

pub struct PpcDisasm;

impl Disassembler for PpcDisasm {
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

	fn bytes_per_unit() -> u16 {
		1
	}

	fn typical_num_units() -> u16 {
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

		assert_eq!(decode_i(0x88000000), Op::RtDRa(Mne::Lbz, Reg::Gpr(0), 0, Reg::LiteralZero));
	}

	#[test]
	fn decode_lbzu() {
		assert_eq!(decode_i(0x8D23FFFF), Op::RtDRa(Mne::Lbzu, Reg::Gpr(9), -1, Reg::Gpr(3)));

		assert_eq!(decode_i(0x8C000000), Op::RtDRa(Mne::Lbzu, Reg::Gpr(0), 0, Reg::Gpr(0)));
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

		assert_eq!(decode_i(0xA8000000), Op::RtDRa(Mne::Lha, Reg::Gpr(0), 0, Reg::LiteralZero));
	}

	#[test]
	fn decode_lhau() {
		assert_eq!(decode_i(0xACE80002), Op::RtDRa(Mne::Lhau, Reg::Gpr(7), 2, Reg::Gpr(8)));
		assert_eq!(decode_i(0xACC40002), Op::RtDRa(Mne::Lhau, Reg::Gpr(6), 2, Reg::Gpr(4)));


		assert_eq!(decode_i(0xAC000000), Op::RtDRa(Mne::Lhau, Reg::Gpr(0), 0, Reg::Gpr(0)));
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

		assert_eq!(decode_i(0xA0000000), Op::RtDRa(Mne::Lhz, Reg::Gpr(0), 0, Reg::LiteralZero));
	}

	#[test]
	fn decode_lhzu() {
		assert_eq!(decode_i(0xA4E80002), Op::RtDRa(Mne::Lhzu, Reg::Gpr(7), 2, Reg::Gpr(8)));

		assert_eq!(decode_i(0xA4000000), Op::RtDRa(Mne::Lhzu, Reg::Gpr(0), 0, Reg::Gpr(0)));
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

	#[test]
	fn decode_lwz() {
		assert_eq!(decode_i(0x812985B0), Op::RtDRa(Mne::Lwz, Reg::Gpr(9), -31312, Reg::Gpr(9)));
		assert_eq!(decode_i(0x80890008), Op::RtDRa(Mne::Lwz, Reg::Gpr(4),      8, Reg::Gpr(9)));

		assert_eq!(decode_i(0x80000000), Op::RtDRa(Mne::Lwz, Reg::Gpr(0), 0, Reg::LiteralZero));
	}

	#[test]
	fn decode_lwzu() {
		assert_eq!(decode_i(0x847F0004), Op::RtDRa(Mne::Lwzu, Reg::Gpr(3), 4, Reg::Gpr(31)));
		assert_eq!(decode_i(0x85060004), Op::RtDRa(Mne::Lwzu, Reg::Gpr(8), 4, Reg::Gpr( 6)));

		assert_eq!(decode_i(0x84000000), Op::RtDRa(Mne::Lwzu, Reg::Gpr(0), 0, Reg::Gpr(0)));
	}

	#[test]
	fn decode_lwzux() {
		assert_eq!(decode_i(0x7D3F486E), Op::RtRaRb(Mne::Lwzux, Reg::Gpr(9), Reg::Gpr(31), Reg::Gpr(9)));

		assert_eq!(decode_i(0x7C00006E), Op::RtRaRb(Mne::Lwzux, Reg::Gpr(0), Reg::Gpr(0), Reg::Gpr(0)));
	}

	#[test]
	fn decode_lwzx() {
		assert_eq!(decode_i(0x7F8A482E), Op::RtRaRb(Mne::Lwzx, Reg::Gpr(28), Reg::Gpr(10), Reg::Gpr( 9)));
		assert_eq!(decode_i(0x7D09502E), Op::RtRaRb(Mne::Lwzx, Reg::Gpr( 8), Reg::Gpr( 9), Reg::Gpr(10)));

		assert_eq!(decode_i(0x7C00002E), Op::RtRaRb(Mne::Lwzx, Reg::Gpr(0), Reg::LiteralZero, Reg::Gpr(0)));
	}
}

