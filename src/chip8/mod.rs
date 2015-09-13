use super::{Addr, Disassembler, DisError, DisResult};

#[derive(Debug, PartialEq)]
pub enum Reg {
	V(u8),
	I,
	Dt,
	K,
	St,
	F,
	B,
}

#[derive(Debug, PartialEq)]
pub enum ImpliedOp {
	Cls,
	Ret,
}

#[derive(Debug, PartialEq)]
pub enum AddrOp {
	Sys,
	Jp,
	Call,
}

#[derive(Debug, PartialEq)]
pub enum RegImmU8Op {
	Se,
	Sne,
	Ld,
	Add,
	Rnd,
}

#[derive(Debug, PartialEq)]
pub enum RegRegOp {
	Se,
	Ld,
	Or,
	And,
	Xor,
	Add,
	Sub,
	Subn,
	Sne,
}

#[derive(Debug, PartialEq)]
pub enum RegSortaOp {
	Shr,
	Shl,
}

#[derive(Debug, PartialEq)]
pub enum RegAddrOp {
	Ld,
	Jp,
}

#[derive(Debug, PartialEq)]
pub enum RegRegImmU4Op {
	Drw,
}

#[derive(Debug, PartialEq)]
pub enum RegOp {
	Skp,
	Sknp,
}

#[derive(Debug, PartialEq)]
pub enum IndirectRegOp {
	Ld,
}

#[derive(Debug, PartialEq)]
pub enum RegIndirectOp {
	Ld,
}

#[derive(Debug, PartialEq)]
pub enum Op {
	Implied(ImpliedOp),
	Addr(AddrOp, u16),
	RegImmU8(RegImmU8Op, Reg, u8),
	RegReg(RegRegOp, Reg, Reg),
	RegSorta(RegSortaOp, Reg, Reg),
	RegAddr(RegAddrOp, Reg, u16),
	RegRegImmU4(RegRegImmU4Op, Reg, Reg, u8),
	Reg(RegOp, Reg),
	IndirectReg(IndirectRegOp, Reg),
	RegIndirect(RegIndirectOp, Reg),
}

pub type DecodeResult = Result<Op, DisError>;

fn addr_of(instr: u16) -> u16 {
	instr & 0x0FFF
}

fn vx_of(instr: u16) -> u8 {
	((instr & 0x0F00) >> 8) as u8
}

fn vy_of(instr: u16) -> u8 {
	((instr & 0x00F0) >> 4) as u8
}

fn kk_of(instr: u16) -> u8 {
	instr as u8
}

fn k_of(instr: u16) -> u8 {
	(instr & 0x000F) as u8
}

pub fn decode(instr: u16) -> DecodeResult {
	let op = match instr >> 12 {
		0x0 => match instr {
			0x00E0 => Op::Implied(ImpliedOp::Cls),
			0x00EE => Op::Implied(ImpliedOp::Ret),
			_      => Op::Addr(AddrOp::Sys, addr_of(instr)),
		},

		0x1 => Op::Addr(AddrOp::Jp, addr_of(instr)),

		0x2 => Op::Addr(AddrOp::Call, addr_of(instr)),

		0x3 => Op::RegImmU8(RegImmU8Op::Se, Reg::V(vx_of(instr)), kk_of(instr)),

		0x4 => Op::RegImmU8(RegImmU8Op::Sne, Reg::V(vx_of(instr)), kk_of(instr)),

		0x5 => match instr & 0x000F {
			0x0 => Op::RegReg(RegRegOp::Se, Reg::V(vx_of(instr)), Reg::V(vy_of(instr))),
			_   => return Err(DisError::Unknown{num_bytes: 2}),
		},

		0x6 => Op::RegImmU8(RegImmU8Op::Ld, Reg::V(vx_of(instr)), kk_of(instr)),

		0x7 => Op::RegImmU8(RegImmU8Op::Add, Reg::V(vx_of(instr)), kk_of(instr)),

		0x8 => match instr & 0x000F {
			0x0 => Op::RegReg(RegRegOp::Ld,  Reg::V(vx_of(instr)), Reg::V(vy_of(instr))),
			0x1 => Op::RegReg(RegRegOp::Or,  Reg::V(vx_of(instr)), Reg::V(vy_of(instr))),
			0x2 => Op::RegReg(RegRegOp::And, Reg::V(vx_of(instr)), Reg::V(vy_of(instr))),
			0x3 => Op::RegReg(RegRegOp::Xor, Reg::V(vx_of(instr)), Reg::V(vy_of(instr))),
			0x4 => Op::RegReg(RegRegOp::Add, Reg::V(vx_of(instr)), Reg::V(vy_of(instr))),
			0x5 => Op::RegReg(RegRegOp::Sub, Reg::V(vx_of(instr)), Reg::V(vy_of(instr))),
			0x6 => Op::RegSorta(RegSortaOp::Shr, Reg::V(vx_of(instr)), Reg::V(vy_of(instr))),
			0x7 => Op::RegReg(RegRegOp::Subn, Reg::V(vx_of(instr)), Reg::V(vy_of(instr))),
			0xE => Op::RegSorta(RegSortaOp::Shl, Reg::V(vx_of(instr)), Reg::V(vy_of(instr))),
			_   => return Err(DisError::Unknown{num_bytes: 2}),
		},

		0x9 => match instr & 0x000F {
			0x0 => Op::RegReg(RegRegOp::Sne, Reg::V(vx_of(instr)), Reg::V(vy_of(instr))),
			_   => return Err(DisError::Unknown{num_bytes: 2}),
		},

		0xA => Op::RegAddr(RegAddrOp::Ld, Reg::I, addr_of(instr)),

		0xB => Op::RegAddr(RegAddrOp::Jp, Reg::V(0), addr_of(instr)),

		0xC => Op::RegImmU8(RegImmU8Op::Rnd, Reg::V(vx_of(instr)), kk_of(instr)),

		0xD => Op::RegRegImmU4(RegRegImmU4Op::Drw, Reg::V(vx_of(instr)), Reg::V(vy_of(instr)), k_of(instr)),

		0xE => match kk_of(instr) {
			0x9E => Op::Reg(RegOp::Skp,  Reg::V(vx_of(instr))),
			0xA1 => Op::Reg(RegOp::Sknp, Reg::V(vx_of(instr))),
			_    => return Err(DisError::Unknown{num_bytes: 2}),
		},

		0xF => match kk_of(instr) {
			0x07 => Op::RegReg(RegRegOp::Ld,  Reg::V(vx_of(instr)), Reg::Dt),
			0x0A => Op::RegReg(RegRegOp::Ld,  Reg::V(vx_of(instr)), Reg::K),
			0x15 => Op::RegReg(RegRegOp::Ld,  Reg::Dt,              Reg::V(vx_of(instr))),
			0x18 => Op::RegReg(RegRegOp::Ld,  Reg::St,              Reg::V(vx_of(instr))),
			0x1E => Op::RegReg(RegRegOp::Add, Reg::I,               Reg::V(vx_of(instr))),
			0x29 => Op::RegReg(RegRegOp::Ld,  Reg::F,               Reg::V(vx_of(instr))),
			0x33 => Op::RegReg(RegRegOp::Ld,  Reg::B,               Reg::V(vx_of(instr))),
			0x55 => Op::IndirectReg(IndirectRegOp::Ld, Reg::V(vx_of(instr))),
			0x65 => Op::RegIndirect(RegIndirectOp::Ld, Reg::V(vx_of(instr))),
			_    => return Err(DisError::Unknown{num_bytes: 2}),
		},

		_ => return Err(DisError::Unknown{num_bytes: 2}),
	};

	Ok(op)
}

pub struct Chip8Disasm;

impl Disassembler for Chip8Disasm {
	#[allow(unused_variables)]
	fn disassemble(&self, addr: Addr, buf: &[u8]) -> DisResult {
		if addr & 1 != 0 {
			return Err(DisError::Unaligned{desired_alignment: 2});
		}

		if addr >= 0x0FFE {
			return Err(DisError::AddrOutOfRange{max_addr: 0x0FFE});
		}

		if buf.len() < 2 {
			return Err(DisError::MemOverflow);
		}

		let instr: u16 = ((buf[0] as u16) << 8) | (buf[1] as u16);

		let op = try!(decode(instr));

		Err(DisError::Unknown{num_bytes: 2})
	}

	fn op_num_bytes_hint(&self) -> u8 {
		2
	}
}

#[cfg(test)]
mod tests {
	use super::*;
	use super::super::DisError;

	#[test]
	fn decode_add() {
		assert_eq!(decode(0x70FF).unwrap(), Op::RegImmU8(RegImmU8Op::Add, Reg::V(0x0), 0xFF));
		assert_eq!(decode(0x7F00).unwrap(), Op::RegImmU8(RegImmU8Op::Add, Reg::V(0xF), 0x00));

		assert_eq!(decode(0x80F4).unwrap(), Op::RegReg(RegRegOp::Add, Reg::V(0x0), Reg::V(0xF)));
		assert_eq!(decode(0x8F04).unwrap(), Op::RegReg(RegRegOp::Add, Reg::V(0xF), Reg::V(0x0)));

		assert_eq!(decode(0xF01E).unwrap(), Op::RegReg(RegRegOp::Add, Reg::I, Reg::V(0x0)));
		assert_eq!(decode(0xFF1E).unwrap(), Op::RegReg(RegRegOp::Add, Reg::I, Reg::V(0xF)));
	}

	#[test]
	fn decode_and() {
		assert_eq!(decode(0x80F2).unwrap(), Op::RegReg(RegRegOp::And, Reg::V(0x0), Reg::V(0xF)));
		assert_eq!(decode(0x8F02).unwrap(), Op::RegReg(RegRegOp::And, Reg::V(0xF), Reg::V(0x0)));
	}

	#[test]
	fn decode_call() {
		assert_eq!(decode(0x2000).unwrap(), Op::Addr(AddrOp::Call, 0x000));
		assert_eq!(decode(0x2423).unwrap(), Op::Addr(AddrOp::Call, 0x423));
		assert_eq!(decode(0x245C).unwrap(), Op::Addr(AddrOp::Call, 0x45C));
		assert_eq!(decode(0x25E0).unwrap(), Op::Addr(AddrOp::Call, 0x5E0));
		assert_eq!(decode(0x27DB).unwrap(), Op::Addr(AddrOp::Call, 0x7DB));
		assert_eq!(decode(0x2958).unwrap(), Op::Addr(AddrOp::Call, 0x958));
		assert_eq!(decode(0x2FFF).unwrap(), Op::Addr(AddrOp::Call, 0xFFF));
	}

	#[test]
	fn decode_cls() {
		assert_eq!(decode(0x00E0).unwrap(), Op::Implied(ImpliedOp::Cls));
	}

	#[test]
	fn decode_drw() {
		assert_eq!(decode(0xD00F).unwrap(), Op::RegRegImmU4(RegRegImmU4Op::Drw, Reg::V(0x0), Reg::V(0x0), 0xF));
		assert_eq!(decode(0xD0F0).unwrap(), Op::RegRegImmU4(RegRegImmU4Op::Drw, Reg::V(0x0), Reg::V(0xF), 0x0));
		assert_eq!(decode(0xDF00).unwrap(), Op::RegRegImmU4(RegRegImmU4Op::Drw, Reg::V(0xF), Reg::V(0x0), 0x0));
	}

	#[test]
	fn decode_jp() {
		assert_eq!(decode(0x1000).unwrap(), Op::Addr(AddrOp::Jp, 0x000));
		assert_eq!(decode(0x1423).unwrap(), Op::Addr(AddrOp::Jp, 0x423));
		assert_eq!(decode(0x145C).unwrap(), Op::Addr(AddrOp::Jp, 0x45C));
		assert_eq!(decode(0x15E0).unwrap(), Op::Addr(AddrOp::Jp, 0x5E0));
		assert_eq!(decode(0x17DB).unwrap(), Op::Addr(AddrOp::Jp, 0x7DB));
		assert_eq!(decode(0x1958).unwrap(), Op::Addr(AddrOp::Jp, 0x958));
		assert_eq!(decode(0x1FFF).unwrap(), Op::Addr(AddrOp::Jp, 0xFFF));

		assert_eq!(decode(0xB000).unwrap(), Op::RegAddr(RegAddrOp::Jp, Reg::V(0), 0x000));
		assert_eq!(decode(0xBD72).unwrap(), Op::RegAddr(RegAddrOp::Jp, Reg::V(0), 0xD72));
		assert_eq!(decode(0xBFFF).unwrap(), Op::RegAddr(RegAddrOp::Jp, Reg::V(0), 0xFFF));
	}

	#[test]
	fn decode_ld() {
		assert_eq!(decode(0x60FF).unwrap(), Op::RegImmU8(RegImmU8Op::Ld, Reg::V(0x0), 0xFF));
		assert_eq!(decode(0x6F00).unwrap(), Op::RegImmU8(RegImmU8Op::Ld, Reg::V(0xF), 0x00));

		assert_eq!(decode(0x80F0).unwrap(), Op::RegReg(RegRegOp::Ld, Reg::V(0x0), Reg::V(0xF)));
		assert_eq!(decode(0x8F00).unwrap(), Op::RegReg(RegRegOp::Ld, Reg::V(0xF), Reg::V(0x0)));

		assert_eq!(decode(0xA000).unwrap(), Op::RegAddr(RegAddrOp::Ld, Reg::I, 0x000));
		assert_eq!(decode(0xA69E).unwrap(), Op::RegAddr(RegAddrOp::Ld, Reg::I, 0x69E));
		assert_eq!(decode(0xAFFF).unwrap(), Op::RegAddr(RegAddrOp::Ld, Reg::I, 0xFFF));

		assert_eq!(decode(0xF007).unwrap(), Op::RegReg(RegRegOp::Ld, Reg::V(0x0), Reg::Dt));
		assert_eq!(decode(0xFF07).unwrap(), Op::RegReg(RegRegOp::Ld, Reg::V(0xF), Reg::Dt));

		assert_eq!(decode(0xF00A).unwrap(), Op::RegReg(RegRegOp::Ld, Reg::V(0x0), Reg::K));
		assert_eq!(decode(0xFF0A).unwrap(), Op::RegReg(RegRegOp::Ld, Reg::V(0xF), Reg::K));

		assert_eq!(decode(0xF015).unwrap(), Op::RegReg(RegRegOp::Ld, Reg::Dt, Reg::V(0x0)));
		assert_eq!(decode(0xFF15).unwrap(), Op::RegReg(RegRegOp::Ld, Reg::Dt, Reg::V(0xF)));

		assert_eq!(decode(0xF018).unwrap(), Op::RegReg(RegRegOp::Ld, Reg::St, Reg::V(0x0)));
		assert_eq!(decode(0xFF18).unwrap(), Op::RegReg(RegRegOp::Ld, Reg::St, Reg::V(0xF)));

		assert_eq!(decode(0xF029).unwrap(), Op::RegReg(RegRegOp::Ld, Reg::F, Reg::V(0x0)));
		assert_eq!(decode(0xFF29).unwrap(), Op::RegReg(RegRegOp::Ld, Reg::F, Reg::V(0xF)));

		assert_eq!(decode(0xF033).unwrap(), Op::RegReg(RegRegOp::Ld, Reg::B, Reg::V(0x0)));
		assert_eq!(decode(0xFF33).unwrap(), Op::RegReg(RegRegOp::Ld, Reg::B, Reg::V(0xF)));

		assert_eq!(decode(0xF055).unwrap(), Op::IndirectReg(IndirectRegOp::Ld, Reg::V(0x0)));
		assert_eq!(decode(0xFF55).unwrap(), Op::IndirectReg(IndirectRegOp::Ld, Reg::V(0xF)));

		assert_eq!(decode(0xF065).unwrap(), Op::RegIndirect(RegIndirectOp::Ld, Reg::V(0x0)));
		assert_eq!(decode(0xFF65).unwrap(), Op::RegIndirect(RegIndirectOp::Ld, Reg::V(0xF)));
	}

	#[test]
	fn decode_or() {
		assert_eq!(decode(0x80F1).unwrap(), Op::RegReg(RegRegOp::Or, Reg::V(0x0), Reg::V(0xF)));
		assert_eq!(decode(0x8F01).unwrap(), Op::RegReg(RegRegOp::Or, Reg::V(0xF), Reg::V(0x0)));
	}

	#[test]
	fn decode_ret() {
		assert_eq!(decode(0x00EE).unwrap(), Op::Implied(ImpliedOp::Ret));
	}

	#[test]
	fn decode_rnd() {
		assert_eq!(decode(0xC0FF).unwrap(), Op::RegImmU8(RegImmU8Op::Rnd, Reg::V(0x0), 0xFF));
		assert_eq!(decode(0xCF00).unwrap(), Op::RegImmU8(RegImmU8Op::Rnd, Reg::V(0xF), 0x00));
	}

	#[test]
	fn decode_se() {
		assert_eq!(decode(0x3000).unwrap(), Op::RegImmU8(RegImmU8Op::Se, Reg::V(0x0), 0x00));
		assert_eq!(decode(0x3567).unwrap(), Op::RegImmU8(RegImmU8Op::Se, Reg::V(0x5), 0x67));
		assert_eq!(decode(0x3FFF).unwrap(), Op::RegImmU8(RegImmU8Op::Se, Reg::V(0xF), 0xFF));

		assert_eq!(decode(0x50F0).unwrap(), Op::RegReg(RegRegOp::Se, Reg::V(0x0), Reg::V(0xF)));
		assert_eq!(decode(0x5F00).unwrap(), Op::RegReg(RegRegOp::Se, Reg::V(0xF), Reg::V(0x0)));
	}

	#[test]
	fn decode_shl() {
		assert_eq!(decode(0x80FE).unwrap(), Op::RegSorta(RegSortaOp::Shl, Reg::V(0x0), Reg::V(0xF)));
		assert_eq!(decode(0x8F0E).unwrap(), Op::RegSorta(RegSortaOp::Shl, Reg::V(0xF), Reg::V(0x0)));
	}

	#[test]
	fn decode_shr() {
		assert_eq!(decode(0x80F6).unwrap(), Op::RegSorta(RegSortaOp::Shr, Reg::V(0x0), Reg::V(0xF)));
		assert_eq!(decode(0x8F06).unwrap(), Op::RegSorta(RegSortaOp::Shr, Reg::V(0xF), Reg::V(0x0)));
	}

	#[test]
	fn decode_sknp() {
		assert_eq!(decode(0xE0A1).unwrap(), Op::Reg(RegOp::Sknp, Reg::V(0x0)));
		assert_eq!(decode(0xEFA1).unwrap(), Op::Reg(RegOp::Sknp, Reg::V(0xF)));
	}

	#[test]
	fn decode_skp() {
		assert_eq!(decode(0xE09E).unwrap(), Op::Reg(RegOp::Skp, Reg::V(0x0)));
		assert_eq!(decode(0xEF9E).unwrap(), Op::Reg(RegOp::Skp, Reg::V(0xF)));
	}

	#[test]
	fn decode_sne() {
		assert_eq!(decode(0x4000).unwrap(), Op::RegImmU8(RegImmU8Op::Sne, Reg::V(0), 0x00));
		assert_eq!(decode(0x4A45).unwrap(), Op::RegImmU8(RegImmU8Op::Sne, Reg::V(0xA), 0x45));
		assert_eq!(decode(0x4FFF).unwrap(), Op::RegImmU8(RegImmU8Op::Sne, Reg::V(0xF), 0xFF));

		assert_eq!(decode(0x90F0).unwrap(), Op::RegReg(RegRegOp::Sne, Reg::V(0x0), Reg::V(0xF)));
		assert_eq!(decode(0x9F00).unwrap(), Op::RegReg(RegRegOp::Sne, Reg::V(0xF), Reg::V(0x0)));
	}

	#[test]
	fn decode_sub() {
		assert_eq!(decode(0x80F5).unwrap(), Op::RegReg(RegRegOp::Sub, Reg::V(0x0), Reg::V(0xF)));
		assert_eq!(decode(0x8F05).unwrap(), Op::RegReg(RegRegOp::Sub, Reg::V(0xF), Reg::V(0x0)));
	}

	#[test]
	fn decode_subn() {
		assert_eq!(decode(0x80F7).unwrap(), Op::RegReg(RegRegOp::Subn, Reg::V(0x0), Reg::V(0xF)));
		assert_eq!(decode(0x8F07).unwrap(), Op::RegReg(RegRegOp::Subn, Reg::V(0xF), Reg::V(0x0)));
	}

	#[test]
	fn decode_sys() {
		assert_eq!(decode(0x0000).unwrap(), Op::Addr(AddrOp::Sys, 0x00));
		assert_eq!(decode(0x0010).unwrap(), Op::Addr(AddrOp::Sys, 0x10));
		assert_eq!(decode(0x00DF).unwrap(), Op::Addr(AddrOp::Sys, 0xDF));
		assert_eq!(decode(0x00F0).unwrap(), Op::Addr(AddrOp::Sys, 0xF0));
		assert_eq!(decode(0x00FF).unwrap(), Op::Addr(AddrOp::Sys, 0xFF));
	}

	#[test]
	fn decode_unknown() {
		assert_eq!(decode(0x5001), Err(DisError::Unknown{num_bytes: 2}));
		assert_eq!(decode(0x5003), Err(DisError::Unknown{num_bytes: 2}));
		assert_eq!(decode(0x500F), Err(DisError::Unknown{num_bytes: 2}));

		assert_eq!(decode(0x8008), Err(DisError::Unknown{num_bytes: 2}));
		assert_eq!(decode(0x8009), Err(DisError::Unknown{num_bytes: 2}));
		assert_eq!(decode(0x800A), Err(DisError::Unknown{num_bytes: 2}));
		assert_eq!(decode(0x800B), Err(DisError::Unknown{num_bytes: 2}));
		assert_eq!(decode(0x800C), Err(DisError::Unknown{num_bytes: 2}));
		assert_eq!(decode(0x800D), Err(DisError::Unknown{num_bytes: 2}));
		assert_eq!(decode(0x800F), Err(DisError::Unknown{num_bytes: 2}));
	}

	#[test]
	fn decode_xor() {
		assert_eq!(decode(0x80F3).unwrap(), Op::RegReg(RegRegOp::Xor, Reg::V(0x0), Reg::V(0xF)));
		assert_eq!(decode(0x8F03).unwrap(), Op::RegReg(RegRegOp::Xor, Reg::V(0xF), Reg::V(0x0)));
	}
}

