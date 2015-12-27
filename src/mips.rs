use super::{Addr, AddrTarget, Disassembler, DisError, DisResult};

pub enum Uarch {
	LsiR2000,       // Canonical MIPS-I processor
	HarvardMips161, // R2000 w/ LL, SC, and WAIT instructions
}

#[allow(dead_code)]
pub struct UarchInfo {
	has_wait_instr: bool,
	has_ll_sc_instr: bool,
}

const LSI_R2000_INFO: &'static UarchInfo = &UarchInfo {
	has_wait_instr: false,
	has_ll_sc_instr: false,
};

const HARVARD_MIPS161_INFO: &'static UarchInfo = &UarchInfo {
	has_wait_instr: false,
	has_ll_sc_instr: false,
};

pub fn uarch_info_for_uarch(uarch: Uarch) -> &'static UarchInfo {
	match uarch {
		Uarch::LsiR2000 => LSI_R2000_INFO,
		Uarch::HarvardMips161 => HARVARD_MIPS161_INFO,
	}
}

pub const ZERO: u8 = 0;
pub const AT:   u8 = 1;
pub const V0:   u8 = 2;
pub const V1:   u8 = 3;
pub const A0:   u8 = 4;
pub const A1:   u8 = 5;
pub const A2:   u8 = 6;
pub const A3:   u8 = 7;
pub const T0:   u8 = 8;
pub const T1:   u8 = 9;
pub const T2:   u8 = 10;
pub const T3:   u8 = 11;
pub const T4:   u8 = 12;
pub const T5:   u8 = 13;
pub const T6:   u8 = 14;
pub const T7:   u8 = 15;
pub const S0:   u8 = 16;
pub const S1:   u8 = 17;
pub const S2:   u8 = 18;
pub const S3:   u8 = 19;
pub const S4:   u8 = 20;
pub const S5:   u8 = 21;
pub const S6:   u8 = 22;
pub const S7:   u8 = 23;
pub const T8:   u8 = 24;
pub const T9:   u8 = 25;
pub const K0:   u8 = 26;
pub const K1:   u8 = 27;
pub const GP:   u8 = 28;
pub const SP:   u8 = 29;
pub const S8:   u8 = 30;
pub const RA:   u8 = 31;

#[derive(Debug, PartialEq)]
pub enum Reg {
	Gpr(u8),
	Cpr(u8),
}

#[derive(Clone, Debug, PartialEq)]
pub enum Cop {
	C0,
	C1,
	C2,
	C3,
}

#[derive(Clone, Debug, PartialEq)]
pub enum Mne {
	Add,
	Addi,
	Addiu,
	Addu,
	And,
	Andi,
	Bct(Cop),
	Bcf(Cop),
	Beq,
	Bgez,
	Bgezal,
	Bgtz,
	Blez,
	Bltz,
	Bltzal,
	Bne,
	Break,
	Cfc(Cop),
	Cop(Cop),
	Ctc(Cop),
	Div,
	Divu,
	J,
	Jal,
	Jalr,
	Jr,
	Lb,
	Lbu,
	Lh,
	Lhu,
	Ll,
	Lui,
	Lw,
	Lwc(Cop),
	Lwl,
	Lwr,
	Mfc(Cop),
	Mfhi,
	Mflo,
	Mtc(Cop),
	Mthi,
	Mtlo,
	Mult,
	Multu,
	Nor,
	Or,
	Ori,
	Sb,
	Sc,
	Sh,
	Sll,
	Sllv,
	Slt,
	Slti,
	Sltiu,
	Sltu,
	Sra,
	Srl,
	Srlv,
	Srav,
	Sub,
	Subu,
	Sw,
	Swc(Cop),
	Swl,
	Syscall,
	Swr,
	Xor,
	Xori,

	//Base Pseudo-ops
	Li,
	Move,
	Nop,

	//COP0 Pseudo-ops
	Rfe,
	Tlbp,
	Tlbr,
	Tlbwi,
	Tlbwr,
	Wait,
}

#[derive(Debug, PartialEq)]
pub enum Op {
	Code(Mne, u32),
	Implied(Mne),
	Rd(Mne, Reg),
	RdRs(Mne, Reg, Reg),
	RdRsRt(Mne, Reg, Reg, Reg),
	RdRtRs(Mne, Reg, Reg, Reg),
	RdRtSa(Mne, Reg, Reg, u8),
	Rs(Mne, Reg),
	RsRt(Mne, Reg, Reg),
	RsRtTarget(Mne, Reg, Reg, AddrTarget),
	RsTarget(Mne, Reg, AddrTarget),
	RtI16(Mne, Reg, i16),
	RtOffsetBase(Mne, Reg, i16, Reg),
	RtRd(Mne, Reg, Reg),
	RtRs(Mne, Reg, Reg),
	RtRsI16(Mne, Reg, Reg, i16),
	RtRsU16(Mne, Reg, Reg, u16),
	RtU16(Mne, Reg, u16),
	Target(Mne, AddrTarget),
}

#[allow(dead_code)]
pub struct DecodeOptions {
	decode_pseudo_ops: bool,
}

fn opcode(instr: u32) -> u8 {
	((instr >> 26) & 0x3F) as u8
}

fn cop_function(instr: u32) -> u8 {
	((instr >> 21) & 0x1F) as u8
}

fn special_function(instr: u32) -> u8 {
	(instr & 0x3F) as u8
}

fn regimm_function(instr: u32) -> u8 {
	((instr >> 16) & 0x1F) as u8
}

fn rs(instr: u32) -> Reg {
	Reg::Gpr(((instr >> 21) & 0x1F) as u8)
}

fn rt(instr: u32) -> Reg {
	Reg::Gpr(((instr >> 16) & 0x1F) as u8)
}

fn crd(instr: u32) -> Reg {
	Reg::Cpr(((instr >> 11) & 0x1F) as u8)
}

fn rd(instr: u32) -> Reg {
	Reg::Gpr(((instr >> 11) & 0x1F) as u8)
}

fn sa(instr: u32) -> u8 {
	((instr >> 6) & 0x1F) as u8
}

fn immi16(instr: u32) -> i16 {
	(instr & 0xFFFF) as i16
}

fn immu16(instr: u32) -> u16 {
	instr as u16
}

fn code(instr: u32) -> u32 {
	((instr >> 16) & 0x3FF) | ((instr << 4) & 0xFFC00)
}

fn cond_branch_offset(instr: u32) -> AddrTarget {
	AddrTarget::Relative((immi16(instr) as i64) << 2)
}

fn jump_target(instr: u32, addr: Addr) -> AddrTarget {
	AddrTarget::Absolute( (((instr & 0x03FF_FFFF) << 2) as u64) | (addr & 0xFFFF_FFFF_F000_0000_u64) )
}

#[allow(unused_variables)]
fn decode_special(instr: u32, uarch_info: &UarchInfo, decode_options: &DecodeOptions) -> Result<Op, DisError> {
	let op = match special_function(instr) {
		0b000000 => Op::RdRtSa(Mne::Sll,  rd(instr), rt(instr), sa(instr)),

		0b000010 => Op::RdRtSa(Mne::Srl,  rd(instr), rt(instr), sa(instr)),
		0b000011 => Op::RdRtSa(Mne::Sra,  rd(instr), rt(instr), sa(instr)),
		0b000100 => Op::RdRtRs(Mne::Sllv, rd(instr), rt(instr), rs(instr)),

		0b000110 => Op::RdRtRs(Mne::Srlv, rd(instr), rt(instr), rs(instr)),
		0b000111 => Op::RdRtRs(Mne::Srav, rd(instr), rt(instr), rs(instr)),
		0b001000 => Op::Rs(Mne::Jr, rs(instr)),
		0b001001 => Op::RdRs(Mne::Jalr, rd(instr), rs(instr)),

		0b001100 => Op::Code(Mne::Syscall, code(instr)),
		0b001101 => Op::Code(Mne::Break,   code(instr)),

		0b010000 => Op::Rd(Mne::Mfhi, rd(instr)),
		0b010001 => Op::Rs(Mne::Mthi, rs(instr)),
		0b010010 => Op::Rd(Mne::Mflo, rd(instr)),
		0b010011 => Op::Rs(Mne::Mtlo, rs(instr)),

		0b011000 => Op::RsRt(Mne::Mult,  rs(instr), rt(instr)),
		0b011001 => Op::RsRt(Mne::Multu, rs(instr), rt(instr)),
		0b011010 => Op::RsRt(Mne::Div,   rs(instr), rt(instr)),
		0b011011 => Op::RsRt(Mne::Divu,  rs(instr), rt(instr)),

		0b100000 => Op::RdRsRt(Mne::Add,  rd(instr), rs(instr), rt(instr)),
		0b100001 => Op::RdRsRt(Mne::Addu, rd(instr), rs(instr), rt(instr)),
		0b100010 => Op::RdRsRt(Mne::Sub,  rd(instr), rs(instr), rt(instr)),
		0b100011 => Op::RdRsRt(Mne::Subu, rd(instr), rs(instr), rt(instr)),
		0b100100 => Op::RdRsRt(Mne::And,  rd(instr), rs(instr), rt(instr)),
		0b100101 => Op::RdRsRt(Mne::Or,   rd(instr), rs(instr), rt(instr)),
		0b100110 => Op::RdRsRt(Mne::Xor,  rd(instr), rs(instr), rt(instr)),
		0b100111 => Op::RdRsRt(Mne::Nor,  rd(instr), rs(instr), rt(instr)),

		0b101010 => Op::RdRsRt(Mne::Slt,  rd(instr), rs(instr), rt(instr)),
		0b101011 => Op::RdRsRt(Mne::Sltu, rd(instr), rs(instr), rt(instr)),

		_ => return Err(DisError::Unknown{num_bytes: 4}),
	};

	Ok(op)
}

fn decode_regimm(instr: u32) -> Result<Op, DisError> {
	let op = match regimm_function(instr) {
		0b00000 => Op::RsTarget(Mne::Bltz, rs(instr), cond_branch_offset(instr)),
		0b00001 => Op::RsTarget(Mne::Bgez, rs(instr), cond_branch_offset(instr)),

		_ => return Err(DisError::Unknown{num_bytes: 4}),
	};

	Ok(op)
}

fn mne_for_op(op: &Op) -> Mne {
	let mne_ref = match op {
		&Op::Code(ref mne, _)               => mne,
		&Op::Implied(ref mne)               => mne,
		&Op::Rd(ref mne, _)                 => mne,
		&Op::RdRs(ref mne, _, _)            => mne,
		&Op::RdRsRt(ref mne, _, _, _)       => mne,
		&Op::RdRtRs(ref mne, _, _, _)       => mne,
		&Op::RdRtSa(ref mne, _, _, _)       => mne,
		&Op::Rs(ref mne, _)                 => mne,
		&Op::RsRt(ref mne, _, _)            => mne,
		&Op::RsRtTarget(ref mne, _, _, _)   => mne,
		&Op::RsTarget(ref mne, _, _)        => mne,
		&Op::RtI16(ref mne, _, _)           => mne,
		&Op::RtOffsetBase(ref mne, _, _, _) => mne,
		&Op::RtRd(ref mne, _, _)            => mne,
		&Op::RtRsI16(ref mne, _, _, _)      => mne,
		&Op::RtRs(ref mne, _, _)            => mne,
		&Op::RtRsU16(ref mne, _, _, _)      => mne,
		&Op::RtU16(ref mne, _, _)           => mne,
		&Op::Target(ref mne, _)             => mne,
	};

	mne_ref.clone()
}

fn has_delay_slot(mne: &Mne) -> bool {
	match mne {
		&Mne::Beq  => true,
		&Mne::Bgez => true,
		&Mne::Bgtz => true,
		&Mne::Blez => true,
		&Mne::Bltz => true,
		&Mne::Bne  => true,
		&Mne::J    => true,
		&Mne::Jal  => true,
		&Mne::Jalr => true,
		&Mne::Jr   => true,
		&Mne::Lb   => true,
		&Mne::Lbu  => true,
		&Mne::Lh   => true,
		&Mne::Lhu  => true,
		&Mne::Lw   => true,
		&Mne::Lwl  => true,
		&Mne::Lwr  => true,

		_ => false,
	}
}

fn decode_cop(cop: Cop, instr: u32) -> Result<Op, DisError> {
	let op = match cop_function(instr) {
		0b00100 => Op::RtRd(Mne::Mtc(cop), rt(instr), crd(instr)),

		_ => return Err(DisError::Unknown{num_bytes: 4}),
	};

	Ok(op)
}

fn convert_to_pseudo_op(op: Op) -> Op {
	match op {
		Op::RdRtSa(Mne::Sll, Reg::Gpr(0), Reg::Gpr(0), 0) => Op::Implied(Mne::Nop),

		Op::RdRsRt(Mne::Addu, rd, rs, Reg::Gpr(0)) => Op::RtRs(Mne::Move, rd, rs),

		Op::RtRsI16(Mne::Addiu, rt, Reg::Gpr(0), imm) => Op::RtI16(Mne::Li, rt, imm),

		_ => op,
	}
}

#[allow(unused_variables)]
pub fn decode(instr: u32, addr: Addr, uarch_info: &UarchInfo, decode_options: &DecodeOptions) -> Result<Op, DisError> {
	let op = match opcode(instr) {
		0b000000 => try!(decode_special(instr, uarch_info, decode_options)),
		0b000001 => try!(decode_regimm(instr)),
		0b000010 => Op::Target(Mne::J,   jump_target(instr, addr)),
		0b000011 => Op::Target(Mne::Jal, jump_target(instr, addr)),
		0b000100 => Op::RsRtTarget(Mne::Beq, rs(instr), rt(instr), cond_branch_offset(instr)),
		0b000101 => Op::RsRtTarget(Mne::Bne, rs(instr), rt(instr), cond_branch_offset(instr)),
		0b000110 => match rt(instr) {
			Reg::Gpr(0) => Op::RsTarget(Mne::Blez, rs(instr), cond_branch_offset(instr)),
			_           => return Err(DisError::Unknown{num_bytes: 4}),
		},
		0b000111 => match rt(instr) {
			Reg::Gpr(0) => Op::RsTarget(Mne::Bgtz, rs(instr), cond_branch_offset(instr)),
			_           => return Err(DisError::Unknown{num_bytes: 4}),
		},

		0b001000 => Op::RtRsI16(Mne::Addi,  rt(instr), rs(instr), immi16(instr)),
		0b001001 => Op::RtRsI16(Mne::Addiu, rt(instr), rs(instr), immi16(instr)),
		0b001010 => Op::RtRsI16(Mne::Slti,  rt(instr), rs(instr), immi16(instr)),
		0b001011 => Op::RtRsI16(Mne::Sltiu, rt(instr), rs(instr), immi16(instr)),

		0b001100 => Op::RtRsU16(Mne::Andi, rt(instr), rs(instr), immu16(instr)),
		0b001101 => Op::RtRsU16(Mne::Ori,  rt(instr), rs(instr), immu16(instr)),
		0b001110 => Op::RtRsU16(Mne::Xori, rt(instr), rs(instr), immu16(instr)),
		0b001111 => match rs(instr) {
			Reg::Gpr(0) => Op::RtU16(Mne::Lui, rt(instr), immu16(instr)),
			_           => return Err(DisError::Unknown{num_bytes: 4}),
		},
		0b010000 => try!(decode_cop(Cop::C0, instr)),

		0b100000 => Op::RtOffsetBase(Mne::Lb,  rt(instr), immi16(instr), rs(instr)),
		0b100001 => Op::RtOffsetBase(Mne::Lh,  rt(instr), immi16(instr), rs(instr)),
		0b100010 => Op::RtOffsetBase(Mne::Lwl, rt(instr), immi16(instr), rs(instr)),
		0b100011 => Op::RtOffsetBase(Mne::Lw,  rt(instr), immi16(instr), rs(instr)),
		0b100100 => Op::RtOffsetBase(Mne::Lbu, rt(instr), immi16(instr), rs(instr)),
		0b100101 => Op::RtOffsetBase(Mne::Lhu, rt(instr), immi16(instr), rs(instr)),
		0b100110 => Op::RtOffsetBase(Mne::Lwr, rt(instr), immi16(instr), rs(instr)),

		0b101000 => Op::RtOffsetBase(Mne::Sb,  rt(instr), immi16(instr), rs(instr)),
		0b101001 => Op::RtOffsetBase(Mne::Sh,  rt(instr), immi16(instr), rs(instr)),
		0b101010 => Op::RtOffsetBase(Mne::Swl, rt(instr), immi16(instr), rs(instr)),
		0b101011 => Op::RtOffsetBase(Mne::Sw,  rt(instr), immi16(instr), rs(instr)),

		0b101110 => Op::RtOffsetBase(Mne::Swr, rt(instr), immi16(instr), rs(instr)),

		_ => return Err(DisError::Unknown{num_bytes: 4}),
	};

	if decode_options.decode_pseudo_ops {
		Ok(convert_to_pseudo_op(op))
	}
	else {
		Ok(op)
	}
}

fn cop_to_num(cop: &Cop) -> u8 {
	match cop {
		&Cop::C0 => 0,
		&Cop::C1 => 1,
		&Cop::C2 => 2,
		&Cop::C3 => 3,
	}
}

fn reg_to_str(reg: &Reg) -> String {
	match reg {
		&Reg::Gpr( 0) => "zero",
		&Reg::Gpr( 1) => "at",
		&Reg::Gpr( 2) => "v0",
		&Reg::Gpr( 3) => "v1",
		&Reg::Gpr( 4) => "a0",
		&Reg::Gpr( 5) => "a1",
		&Reg::Gpr( 6) => "a2",
		&Reg::Gpr( 7) => "a3",
		&Reg::Gpr( 8) => "t0",
		&Reg::Gpr( 9) => "t1",
		&Reg::Gpr(10) => "t2",
		&Reg::Gpr(11) => "t3",
		&Reg::Gpr(12) => "t4",
		&Reg::Gpr(13) => "t5",
		&Reg::Gpr(14) => "t6",
		&Reg::Gpr(15) => "t7",
		&Reg::Gpr(16) => "s0",
		&Reg::Gpr(17) => "s1",
		&Reg::Gpr(18) => "s2",
		&Reg::Gpr(19) => "s3",
		&Reg::Gpr(20) => "s4",
		&Reg::Gpr(21) => "s5",
		&Reg::Gpr(22) => "s6",
		&Reg::Gpr(23) => "s7",
		&Reg::Gpr(24) => "t8",
		&Reg::Gpr(25) => "t9",
		&Reg::Gpr(26) => "k0",
		&Reg::Gpr(27) => "k1",
		&Reg::Gpr(28) => "gp",
		&Reg::Gpr(29) => "sp",
		&Reg::Gpr(30) => "s8",
		&Reg::Gpr(31) => "ra",

		&Reg::Cpr(cpr) => return format!("${}", cpr),

		&Reg::Gpr(gpr) => return format!("<error:gpr{}", gpr),
	}.to_string()
}

fn mne_to_str(mne: &Mne) -> String {
	match *mne {
		Mne::And     => "and",
		Mne::Add     => "add",
		Mne::Addi    => "addi",
		Mne::Addiu   => "addiu",
		Mne::Addu    => "addu",
		Mne::Andi    => "andi",
		Mne::Beq     => "beq",
		Mne::Bgez    => "bgez",
		Mne::Bgezal  => "bgezal",
		Mne::Bgtz    => "bgtz",
		Mne::Blez    => "blez",
		Mne::Bltz    => "bltz",
		Mne::Bltzal  => "bltzal",
		Mne::Bne     => "bne",
		Mne::Break   => "break",
		Mne::Div     => "div",
		Mne::Divu    => "divu",
		Mne::J       => "j",
		Mne::Jal     => "jal",
		Mne::Jalr    => "jalr",
		Mne::Jr      => "jr",
		Mne::Lb      => "lb",
		Mne::Lbu     => "lbu",
		Mne::Lh      => "lh",
		Mne::Lhu     => "lhu",
		Mne::Ll      => "ll",
		Mne::Lw      => "lw",
		Mne::Lwl     => "lwl",
		Mne::Lwr     => "lwr",
		Mne::Lui     => "lui",
		Mne::Mfhi    => "mfhi",
		Mne::Mflo    => "mflo",
		Mne::Mthi    => "mthi",
		Mne::Mtlo    => "mtlo",
		Mne::Mult    => "mult",
		Mne::Multu   => "multu",
		Mne::Nor     => "nor",
		Mne::Or      => "or",
		Mne::Ori     => "ori",
		Mne::Sb      => "sb",
		Mne::Sc      => "sc",
		Mne::Sh      => "sh",
		Mne::Sll     => "sll",
		Mne::Sllv    => "sllv",
		Mne::Slt     => "slt",
		Mne::Slti    => "slti",
		Mne::Sltiu   => "sltiu",
		Mne::Sltu    => "sltu",
		Mne::Sra     => "sra",
		Mne::Srav    => "srav",
		Mne::Srl     => "srl",
		Mne::Srlv    => "srlv",
		Mne::Sub     => "sub",
		Mne::Subu    => "subu",
		Mne::Sw      => "sw",
		Mne::Swr     => "swr",
		Mne::Swl     => "swl",
		Mne::Syscall => "syscall",
		Mne::Xor     => "xor",
		Mne::Xori    => "xori",

		Mne::Bcf(ref cop) => return format!("bc{}f", cop_to_num(cop)),
		Mne::Bct(ref cop) => return format!("bc{}t", cop_to_num(cop)),
		Mne::Cfc(ref cop) => return format!("cfc{}", cop_to_num(cop)),
		Mne::Cop(ref cop) => return format!("cop{}", cop_to_num(cop)),
		Mne::Ctc(ref cop) => return format!("ctc{}", cop_to_num(cop)),
		Mne::Lwc(ref cop) => return format!("lwc{}", cop_to_num(cop)),
		Mne::Mfc(ref cop) => return format!("mfc{}", cop_to_num(cop)),
		Mne::Mtc(ref cop) => return format!("mtc{}", cop_to_num(cop)),
		Mne::Swc(ref cop) => return format!("swc{}", cop_to_num(cop)),

		Mne::Li    => "li",
		Mne::Move  => "move",
		Mne::Nop   => "nop",

		Mne::Rfe   => "rfe",
		Mne::Tlbp  => "tlbp",
		Mne::Tlbr  => "tlbr",
		Mne::Tlbwi => "tlbwi",
		Mne::Tlbwr => "tlbwr",
		Mne::Wait  => "wait",
	}.to_string()
}

fn target_to_str(addr: Addr, target: &AddrTarget) -> String {
	match target {
		&AddrTarget::Absolute(abs)   => format!("{:#x}", abs),
		&AddrTarget::Relative(rel)   => format!("{:#x}", rel + (addr as i64) + 4),
		&AddrTarget::Symbol(ref sym) => format!("{}", sym),
	}
}

fn op_to_str(addr: Addr, op: &Op) -> String {
	let (mne, args_str) = match op {
		&Op::Implied(ref mne) => (mne.clone(), None),


		&Op::Code(ref mne, 0) => {
			(mne.clone(), None)
		},

		&Op::Code(ref mne, code) => {
			(mne.clone(), Some(format!("{:#x}", code)))
		},

		&Op::Rd(ref mne, ref rd) => {
			(mne.clone(), Some(format!("{}", reg_to_str(rd))))
		},

		&Op::RdRs(Mne::Jalr, Reg::Gpr(RA), ref rs) => {
			(Mne::Jalr, Some(format!("{}", reg_to_str(rs))))
		},

		&Op::RdRs(ref mne, ref rd, ref rs) => {
			(mne.clone(), Some(format!("{},{}", reg_to_str(rd), reg_to_str(rs))))
		},

		&Op::RdRsRt(ref mne, ref rd, ref rs, ref rt) => {
			(mne.clone(), Some(format!("{},{},{}", reg_to_str(rd), reg_to_str(rs), reg_to_str(rt))))
		},

		&Op::RdRtRs(ref mne, ref rd, ref rt, ref rs) => {
			(mne.clone(), Some(format!("{},{},{}", reg_to_str(rd), reg_to_str(rt), reg_to_str(rs))))
		},

		&Op::RdRtSa(ref mne, ref rd, ref rt, sa) => {
			(mne.clone(), Some(format!("{},{},{:#x}", reg_to_str(rd), reg_to_str(rt), sa)))
		},

		&Op::Rs(ref mne, ref rs) => {
			(mne.clone(), Some(format!("{}", reg_to_str(rs))))
		},

		&Op::RsRt(Mne::Div, ref rs, ref rt) => {
			(Mne::Div, Some(format!("{},{},{}", reg_to_str(&Reg::Gpr(ZERO)), reg_to_str(rs), reg_to_str(rt))))
		},

		&Op::RsRt(Mne::Divu, ref rs, ref rt) => {
			(Mne::Divu, Some(format!("{},{},{}", reg_to_str(&Reg::Gpr(ZERO)), reg_to_str(rs), reg_to_str(rt))))
		},

		&Op::RsRt(ref mne, ref rs, ref rt) => {
			(mne.clone(), Some(format!("{},{}", reg_to_str(rs), reg_to_str(rt))))
		},

		&Op::RsRtTarget(ref mne, ref rs, ref rt, ref target) => {
			(mne.clone(), Some(format!("{},{},{}", reg_to_str(rs), reg_to_str(rt), target_to_str(addr, target))))
		},

		&Op::RsTarget(ref mne, ref rs, ref target) => {
			(mne.clone(), Some(format!("{},{}", reg_to_str(rs), target_to_str(addr, target))))
		},

		&Op::RtI16(ref mne, ref rt, imm) => {
			(mne.clone(), Some(format!("{},{}", reg_to_str(rt), imm)))
		},

		&Op::RtOffsetBase(ref mne, ref rt, offset, ref base) => {
			(mne.clone(), Some(format!("{},{}({})", reg_to_str(rt), offset, reg_to_str(base))))
		},

		&Op::RtRd(ref mne, ref rt, ref rd) => {
			(mne.clone(), Some(format!("{},{}", reg_to_str(rt), reg_to_str(rd))))
		},

		&Op::RtRsI16(ref mne, ref rt, ref rs, imm) => {
			(mne.clone(), Some(format!("{},{},{}", reg_to_str(rt), reg_to_str(rs), imm)))
		},

		&Op::RtRs(ref mne, ref rt, ref rs) => {
			(mne.clone(), Some(format!("{},{}", reg_to_str(rt), reg_to_str(rs))))
		},

		&Op::RtRsU16(ref mne, ref rt, ref rs, imm) => {
			(mne.clone(), Some(format!("{},{},{:#x}", reg_to_str(rt), reg_to_str(rs), imm)))
		},

		&Op::RtU16(ref mne, ref rt, imm) => {
			(mne.clone(), Some(format!("{},{:#x}", reg_to_str(rt), imm)))
		},

		&Op::Target(ref mne, ref target) => {
			(mne.clone(), Some(target_to_str(addr, target)))
		},
	};

	match args_str {
		Some(args) => format!("{:7} {}", mne_to_str(&mne), args),
		None       => format!("{}", mne_to_str(&mne)),
	}
}

#[allow(unused_variables)]
pub fn disasm(addr: Addr, buf: &[u8], uarch_info: &UarchInfo, decode_options: &DecodeOptions) -> DisResult {
	if buf.len() < 4 {
		return Err(DisError::MemOverflow);
	}

	if (addr % 4) != 0 {
		return Err( DisError::Unaligned{ desired_alignment: 4, } );
	}

	let instr = ((buf[0] as u32) << 24) |
	            ((buf[1] as u32) << 16) |
	            ((buf[2] as u32) << 8 ) |
	            ((buf[3] as u32) << 0 );

	let op = try!(decode(instr, addr, uarch_info, decode_options));

	let mne = mne_for_op(&op);

	Ok((op_to_str(addr, &op), 4, has_delay_slot(&mne)))
}

pub struct MipsDisasm;

impl Disassembler for MipsDisasm {
	fn disassemble(&self, addr: Addr, buf: &[u8]) -> DisResult  {
		let uarch_info = uarch_info_for_uarch(Uarch::HarvardMips161);

		let no_pseudo_ops = DecodeOptions { decode_pseudo_ops: true };

		disasm(addr, buf, uarch_info, &no_pseudo_ops)
	}

	fn bytes_per_unit(&self) -> u16 {
		4
	}
}

#[cfg(test)]
mod tests {
	use super::*;

	use super::super::*;

	#[allow(dead_code)]
	enum TestCase {
		Normal{ instr: u32, asm: &'static str, delay: bool, op: Op },
		Branch{ addr: Addr, instr: u32, asm: &'static str, op: Op },
	}

	static BASE_TEST_CASES: [TestCase; 90] = [
		TestCase::Normal{ instr: 0x02024020, asm: "add     t0,s0,v0",        delay: false, op: Op::RdRsRt(Mne::Add, Reg::Gpr(T0), Reg::Gpr(S0), Reg::Gpr(V0)) },

		TestCase::Normal{ instr: 0x03A0F021, asm: "addu    s8,sp,zero",      delay: false, op: Op::RdRsRt(Mne::Addu, Reg::Gpr(S8), Reg::Gpr(SP), Reg::Gpr(ZERO)) },

		TestCase::Normal{ instr: 0x20101F81, asm: "addi    s0,zero,8065",    delay: false, op: Op::RtRsI16(Mne::Addi, Reg::Gpr(S0), Reg::Gpr(ZERO), 8065) },
		TestCase::Normal{ instr: 0x2231FFFF, asm: "addi    s1,s1,-1",        delay: false, op: Op::RtRsI16(Mne::Addi, Reg::Gpr(S1), Reg::Gpr(S1), -1) },

		TestCase::Normal{ instr: 0x27BDFFE8, asm: "addiu   sp,sp,-24",       delay: false, op: Op::RtRsI16(Mne::Addiu, Reg::Gpr(SP), Reg::Gpr(SP), -24) },
		TestCase::Normal{ instr: 0x24020020, asm: "addiu   v0,zero,32",      delay: false, op: Op::RtRsI16(Mne::Addiu, Reg::Gpr(V0), Reg::Gpr(ZERO), 32) },

		TestCase::Normal{ instr: 0x01094024, asm: "and     t0,t0,t1",        delay: false, op: Op::RdRsRt(Mne::And, Reg::Gpr(T0), Reg::Gpr(T0), Reg::Gpr(T1)) },

		TestCase::Normal{ instr: 0x307A0001, asm: "andi    k0,v1,0x1",       delay: false, op: Op::RtRsU16(Mne::Andi, Reg::Gpr(K0), Reg::Gpr(V1), 0x1) },
		TestCase::Normal{ instr: 0x30018000, asm: "andi    at,zero,0x8000",  delay: false, op: Op::RtRsU16(Mne::Andi, Reg::Gpr(AT), Reg::Gpr(ZERO), 0x8000) },

		TestCase::Normal{ instr: 0x0007000D, asm: "break   0x7",             delay: false, op: Op::Code(Mne::Break, 7) },
		TestCase::Normal{ instr: 0x03FF000D, asm: "break   0x3ff",         delay: false, op: Op::Code(Mne::Break, 0x3FF) },
		TestCase::Normal{ instr: 0x0000FFCD, asm: "break   0xffc00",         delay: false, op: Op::Code(Mne::Break, 0xFFC00) },

		TestCase::Normal{ instr: 0x0062001A, asm: "div     zero,v1,v0",      delay: false, op: Op::RsRt(Mne::Div, Reg::Gpr(V1), Reg::Gpr(V0)) },

		TestCase::Normal{ instr: 0x00A2001B, asm: "divu    zero,a1,v0",      delay: false, op: Op::RsRt(Mne::Divu, Reg::Gpr(A1), Reg::Gpr(V0)) },

		TestCase::Normal{ instr: 0x0060F809, asm: "jalr    v1",              delay: true,  op: Op::RdRs(Mne::Jalr, Reg::Gpr(RA), Reg::Gpr(V1)) },
		TestCase::Normal{ instr: 0x00C0F809, asm: "jalr    a2",              delay: true,  op: Op::RdRs(Mne::Jalr, Reg::Gpr(RA), Reg::Gpr(A2)) },
		TestCase::Normal{ instr: 0x00C04809, asm: "jalr    t1,a2",           delay: true,  op: Op::RdRs(Mne::Jalr, Reg::Gpr(T1), Reg::Gpr(A2)) },

		TestCase::Normal{ instr: 0x00400008, asm: "jr      v0",              delay: true,  op: Op::Rs(Mne::Jr, Reg::Gpr(V0)) },
		TestCase::Normal{ instr: 0x03E00008, asm: "jr      ra",              delay: true,  op: Op::Rs(Mne::Jr, Reg::Gpr(RA)) },

		TestCase::Normal{ instr: 0x83C50024, asm: "lb      a1,36(s8)",       delay: true,  op: Op::RtOffsetBase(Mne::Lb, Reg::Gpr(A1), 36, Reg::Gpr(S8)) },
		TestCase::Normal{ instr: 0x80420000, asm: "lb      v0,0(v0)",        delay: true,  op: Op::RtOffsetBase(Mne::Lb, Reg::Gpr(V0),  0, Reg::Gpr(V0)) },

		TestCase::Normal{ instr: 0x90420224, asm: "lbu     v0,548(v0)",      delay: true,  op: Op::RtOffsetBase(Mne::Lbu, Reg::Gpr(V0), 548, Reg::Gpr(V0)) },
		TestCase::Normal{ instr: 0x93C3003F, asm: "lbu     v1,63(s8)",       delay: true,  op: Op::RtOffsetBase(Mne::Lbu, Reg::Gpr(V1),  63, Reg::Gpr(S8)) },

		TestCase::Normal{ instr: 0x87a30042, asm: "lh      v1,66(sp)",       delay: true,  op: Op::RtOffsetBase(Mne::Lh, Reg::Gpr(V1),   66, Reg::Gpr(SP)) },
		TestCase::Normal{ instr: 0x8602FF6E, asm: "lh      v0,-146(s0)",     delay: true,  op: Op::RtOffsetBase(Mne::Lh, Reg::Gpr(V0), -146, Reg::Gpr(S0)) },

		TestCase::Normal{ instr: 0x948A036C, asm: "lhu     t2,876(a0)",      delay: true,  op: Op::RtOffsetBase(Mne::Lhu, Reg::Gpr(T2), 876, Reg::Gpr(A0)) },
		TestCase::Normal{ instr: 0x96450026, asm: "lhu     a1,38(s2)",       delay: true,  op: Op::RtOffsetBase(Mne::Lhu, Reg::Gpr(A1),  38, Reg::Gpr(S2)) },

		TestCase::Normal{ instr: 0x3C0AA470, asm: "lui     t2,0xa470",       delay: false, op: Op::RtU16(Mne::Lui, Reg::Gpr(T2), 0xA470) },
		TestCase::Normal{ instr: 0x3C1F0010, asm: "lui     ra,0x10",         delay: false, op: Op::RtU16(Mne::Lui, Reg::Gpr(RA), 0x10) },

		TestCase::Normal{ instr: 0x8C43BB90, asm: "lw      v1,-17520(v0)",   delay: true,  op: Op::RtOffsetBase(Mne::Lw, Reg::Gpr(V1), -17520, Reg::Gpr(V0)) },
		TestCase::Normal{ instr: 0x8C430000, asm: "lw      v1,0(v0)",        delay: true,  op: Op::RtOffsetBase(Mne::Lw, Reg::Gpr(V1),      0, Reg::Gpr(V0)) },
		TestCase::Normal{ instr: 0x8FC20018, asm: "lw      v0,24(s8)",       delay: true,  op: Op::RtOffsetBase(Mne::Lw, Reg::Gpr(V0),     24, Reg::Gpr(S8)) },
		TestCase::Normal{ instr: 0x8FBF0014, asm: "lw      ra,20(sp)",       delay: true,  op: Op::RtOffsetBase(Mne::Lw, Reg::Gpr(RA),     20, Reg::Gpr(SP)) },

		TestCase::Normal{ instr: 0x88430FE8, asm: "lwl     v1,4072(v0)",     delay: true,  op: Op::RtOffsetBase(Mne::Lwl, Reg::Gpr(V1),   4072, Reg::Gpr(V0)) },
		TestCase::Normal{ instr: 0x88439000, asm: "lwl     v1,-28672(v0)",   delay: true,  op: Op::RtOffsetBase(Mne::Lwl, Reg::Gpr(V1), -28672, Reg::Gpr(V0)) },

		TestCase::Normal{ instr: 0x98430FE3, asm: "lwr     v1,4067(v0)",     delay: true,  op: Op::RtOffsetBase(Mne::Lwr, Reg::Gpr(V1), 4067, Reg::Gpr(V0)) },
		TestCase::Normal{ instr: 0x9A22000B, asm: "lwr     v0,11(s1)",       delay: true,  op: Op::RtOffsetBase(Mne::Lwr, Reg::Gpr(V0),   11, Reg::Gpr(S1)) },

		TestCase::Normal{ instr: 0x00001010, asm: "mfhi    v0",              delay: false, op: Op::Rd(Mne::Mfhi, Reg::Gpr(V0)) },

		TestCase::Normal{ instr: 0x00002012, asm: "mflo    a0",              delay: false, op: Op::Rd(Mne::Mflo, Reg::Gpr(A0)) },

		TestCase::Normal{ instr: 0x40806800, asm: "mtc0    zero,$13",        delay: false, op: Op::RtRd(Mne::Mtc(Cop::C0), Reg::Gpr(0), Reg::Cpr(13)) },
		TestCase::Normal{ instr: 0x40804800, asm: "mtc0    zero,$9",         delay: false, op: Op::RtRd(Mne::Mtc(Cop::C0), Reg::Gpr(0), Reg::Cpr(9)) },
		TestCase::Normal{ instr: 0x40805800, asm: "mtc0    zero,$11",        delay: false, op: Op::RtRd(Mne::Mtc(Cop::C0), Reg::Gpr(0), Reg::Cpr(11)) },

		TestCase::Normal{ instr: 0x01000011, asm: "mthi    t0",              delay: false, op: Op::Rs(Mne::Mthi, Reg::Gpr(T0)) },

		TestCase::Normal{ instr: 0x01200013, asm: "mtlo    t1",              delay: false, op: Op::Rs(Mne::Mtlo, Reg::Gpr(T1)) },

		TestCase::Normal{ instr: 0x00C20018, asm: "mult    a2,v0",           delay: false, op: Op::RsRt(Mne::Mult, Reg::Gpr(A2), Reg::Gpr(V0)) },

		TestCase::Normal{ instr: 0x00820019, asm: "multu   a0,v0",           delay: false, op: Op::RsRt(Mne::Multu, Reg::Gpr(A0), Reg::Gpr(V0)) },

		TestCase::Normal{ instr: 0x00021827, asm: "nor     v1,zero,v0",      delay: false, op: Op::RdRsRt(Mne::Nor, Reg::Gpr(V1), Reg::Gpr(ZERO), Reg::Gpr(V0)) },

		TestCase::Normal{ instr: 0x00c22025, asm: "or      a0,a2,v0",        delay: false, op: Op::RdRsRt(Mne::Or, Reg::Gpr(A0), Reg::Gpr(A2), Reg::Gpr(V0)) },

		TestCase::Normal{ instr: 0x3409010F, asm: "ori     t1,zero,0x10f",   delay: false, op: Op::RtRsU16(Mne::Ori, Reg::Gpr(T1), Reg::Gpr(ZERO), 0x10f) },
		TestCase::Normal{ instr: 0x35714000, asm: "ori     s1,t3,0x4000",    delay: false, op: Op::RtRsU16(Mne::Ori, Reg::Gpr(S1), Reg::Gpr(T3), 0x4000) },

		TestCase::Normal{ instr: 0xA220A020, asm: "sb      zero,-24544(s1)", delay: false, op: Op::RtOffsetBase(Mne::Sb, Reg::Gpr(ZERO), -24544, Reg::Gpr(S1)) },
		TestCase::Normal{ instr: 0xA0433882, asm: "sb      v1,14466(v0)",    delay: false, op: Op::RtOffsetBase(Mne::Sb, Reg::Gpr(V1),    14466, Reg::Gpr(V0)) },

		TestCase::Normal{ instr: 0xA642E6F0, asm: "sh      v0,-6416(s2)",    delay: false, op: Op::RtOffsetBase(Mne::Sh, Reg::Gpr(V0),   -6416, Reg::Gpr(S2)) },
		TestCase::Normal{ instr: 0xA640002A, asm: "sh      zero,42(s2)",     delay: false, op: Op::RtOffsetBase(Mne::Sh, Reg::Gpr(ZERO),    42, Reg::Gpr(S2)) },

		TestCase::Normal{ instr: 0x00021400, asm: "sll     v0,v0,0x10",      delay: false, op: Op::RdRtSa(Mne::Sll, Reg::Gpr(V0), Reg::Gpr(V0), 0x10) },
		TestCase::Normal{ instr: 0x00000000, asm: "sll     zero,zero,0x0",   delay: false, op: Op::RdRtSa(Mne::Sll, Reg::Gpr(ZERO), Reg::Gpr(ZERO), 0) },

		TestCase::Normal{ instr: 0x00621004, asm: "sllv    v0,v0,v1",        delay: false, op: Op::RdRtRs(Mne::Sllv, Reg::Gpr(V0), Reg::Gpr(V0), Reg::Gpr(V1)) },

		TestCase::Normal{ instr: 0x0062102A, asm: "slt     v0,v1,v0",        delay: false, op: Op::RdRsRt(Mne::Slt, Reg::Gpr(V0), Reg::Gpr(V1), Reg::Gpr(V0)) },

		TestCase::Normal{ instr: 0x28620031, asm: "slti    v0,v1,49",        delay: false, op: Op::RtRsI16(Mne::Slti, Reg::Gpr(V0), Reg::Gpr(V1), 49) },

		TestCase::Normal{ instr: 0x2C620015, asm: "sltiu   v0,v1,21",        delay: false, op: Op::RtRsI16(Mne::Sltiu, Reg::Gpr(V0), Reg::Gpr(V1), 21) },

		TestCase::Normal{ instr: 0x0002102B, asm: "sltu    v0,zero,v0",      delay: false, op: Op::RdRsRt(Mne::Sltu, Reg::Gpr(V0), Reg::Gpr(ZERO), Reg::Gpr(V0)) },

		TestCase::Normal{ instr: 0x00042FC3, asm: "sra     a1,a0,0x1f",      delay: false, op: Op::RdRtSa(Mne::Sra, Reg::Gpr(A1), Reg::Gpr(A0), 0x1f) },

		TestCase::Normal{ instr: 0x00431007, asm: "srav    v0,v1,v0",        delay: false, op: Op::RdRtRs(Mne::Srav, Reg::Gpr(V0), Reg::Gpr(V1), Reg::Gpr(V0)) },

		TestCase::Normal{ instr: 0x00021C02, asm: "srl     v1,v0,0x10",      delay: false, op: Op::RdRtSa(Mne::Srl, Reg::Gpr(V1), Reg::Gpr(V0), 0x10) },

		TestCase::Normal{ instr: 0x00433006, asm: "srlv    a2,v1,v0",        delay: false, op: Op::RdRtRs(Mne::Srlv, Reg::Gpr(A2), Reg::Gpr(V1), Reg::Gpr(V0)) },

		TestCase::Normal{ instr: 0x00C53022, asm: "sub     a2,a2,a1",        delay: false, op: Op::RdRsRt(Mne::Sub, Reg::Gpr(A2), Reg::Gpr(A2), Reg::Gpr(A1)) },

		TestCase::Normal{ instr: 0x00A71823, asm: "subu    v1,a1,a3",        delay: false, op: Op::RdRsRt(Mne::Subu, Reg::Gpr(V1), Reg::Gpr(A1), Reg::Gpr(A3)) },

		TestCase::Normal{ instr: 0xAFBF0014, asm: "sw      ra,20(sp)",       delay: false, op: Op::RtOffsetBase(Mne::Sw, Reg::Gpr(RA), 20, Reg::Gpr(SP)) },
		TestCase::Normal{ instr: 0xAFBE0010, asm: "sw      s8,16(sp)",       delay: false, op: Op::RtOffsetBase(Mne::Sw, Reg::Gpr(S8), 16, Reg::Gpr(SP)) },
		TestCase::Normal{ instr: 0xAFC00030, asm: "sw      zero,48(s8)",     delay: false, op: Op::RtOffsetBase(Mne::Sw, Reg::Gpr( 0), 48, Reg::Gpr(S8)) },

		TestCase::Normal{ instr: 0xA8A20FE4, asm: "swl     v0,4068(a1)",     delay: false, op: Op::RtOffsetBase(Mne::Swl, Reg::Gpr(V0), 4068, Reg::Gpr(A1)) },
		TestCase::Normal{ instr: 0xA8400010, asm: "swl     zero,16(v0)",     delay: false, op: Op::RtOffsetBase(Mne::Swl, Reg::Gpr(ZERO), 16, Reg::Gpr(V0)) },

		TestCase::Normal{ instr: 0xB8A20FE7, asm: "swr     v0,4071(a1)",     delay: false, op: Op::RtOffsetBase(Mne::Swr, Reg::Gpr(V0),   4071, Reg::Gpr(A1)) },
		TestCase::Normal{ instr: 0xB8400013, asm: "swr     zero,19(v0)",     delay: false, op: Op::RtOffsetBase(Mne::Swr, Reg::Gpr(ZERO),   19, Reg::Gpr(V0)) },

		TestCase::Normal{ instr: 0x0000000C, asm: "syscall",                 delay: false, op: Op::Code(Mne::Syscall, 0) },
		TestCase::Normal{ instr: 0x0007000C, asm: "syscall 0x7",             delay: false, op: Op::Code(Mne::Syscall, 7) },

		TestCase::Normal{ instr: 0x00621026, asm: "xor     v0,v1,v0",        delay: false, op: Op::RdRsRt(Mne::Xor, Reg::Gpr(V0), Reg::Gpr(V1), Reg::Gpr(V0)) },

		TestCase::Normal{ instr: 0x3884003F, asm: "xori    a0,a0,0x3f",      delay: false, op: Op::RtRsU16(Mne::Xori, Reg::Gpr(A0), Reg::Gpr(A0), 0x3F) },

		TestCase::Branch{ addr: 0x80000368, instr: 0x10620033, asm: "beq     v1,v0,0x80000438", op: Op::RsRtTarget(Mne::Beq, Reg::Gpr(V1), Reg::Gpr(V0), AddrTarget::Relative(204)) },
		TestCase::Branch{ addr: 0x80001424, instr: 0x1062FFF7, asm: "beq     v1,v0,0x80001404", op: Op::RsRtTarget(Mne::Beq, Reg::Gpr(V1), Reg::Gpr(V0), AddrTarget::Relative(-36)) },
		TestCase::Branch{ addr: 0x80001BFC, instr: 0x1082FFE8, asm: "beq     a0,v0,0x80001ba0", op: Op::RsRtTarget(Mne::Beq, Reg::Gpr(A0), Reg::Gpr(V0), AddrTarget::Relative(-96)) },

		TestCase::Branch{ addr: 0x8002987C, instr: 0x0541FFF7, asm: "bgez    t2,0x8002985c",    op: Op::RsTarget(Mne::Bgez, Reg::Gpr(T2), AddrTarget::Relative(-36)) },

		TestCase::Branch{ addr: 0x80010ca4, instr: 0x1C40000A, asm: "bgtz    v0,0x80010cd0",    op: Op::RsTarget(Mne::Bgtz, Reg::Gpr(V0), AddrTarget::Relative(40)) },

		TestCase::Branch{ addr: 0x80000448, instr: 0x1840000D, asm: "blez    v0,0x80000480",    op: Op::RsTarget(Mne::Blez, Reg::Gpr(V0), AddrTarget::Relative(52)) },

		TestCase::Branch{ addr: 0x8000B698, instr: 0x04400003, asm: "bltz    v0,0x8000b6a8",    op: Op::RsTarget(Mne::Bltz, Reg::Gpr(V0), AddrTarget::Relative(12)) },

		TestCase::Branch{ addr: 0x8001CB34, instr: 0x1443000C, asm: "bne     v0,v1,0x8001cb68", op: Op::RsRtTarget(Mne::Bne, Reg::Gpr(V0), Reg::Gpr(V1), AddrTarget::Relative( 48)) },
		TestCase::Branch{ addr: 0x80029890, instr: 0x1501FFF2, asm: "bne     t0,at,0x8002985c", op: Op::RsRtTarget(Mne::Bne, Reg::Gpr(T0), Reg::Gpr(AT), AddrTarget::Relative(-56)) },

		TestCase::Branch{ addr: 0x80000914, instr: 0x0800024a, asm: "j       0x80000928",       op: Op::Target(Mne::J, AddrTarget::Absolute(0x80000928)) },

		TestCase::Branch{ addr: 0x80029DE8, instr: 0x0C000730, asm: "jal     0x80001cc0",       op: Op::Target(Mne::Jal, AddrTarget::Absolute(0x80001CC0)) },
		TestCase::Branch{ addr: 0x80029EA4, instr: 0x0C004040, asm: "jal     0x80010100",       op: Op::Target(Mne::Jal, AddrTarget::Absolute(0x80010100)) },
	];

	#[test]
	fn decode_base() {
		let no_pseudo_ops = DecodeOptions { decode_pseudo_ops: false };

		let uarch_info = uarch_info_for_uarch(Uarch::HarvardMips161);

		for test_case in BASE_TEST_CASES.iter() {
			match test_case {
				&TestCase::Normal{instr, ref op, ..} => {
					assert_eq!(&decode(instr, 0, uarch_info, &no_pseudo_ops).unwrap(), op)
				},

				&TestCase::Branch{addr, instr, ref op, ..} => {
					assert_eq!(&decode(instr, addr, uarch_info, &no_pseudo_ops).unwrap(), op)
				},
			}
		}
	}

	#[test]
	fn disasm_base() {
		let no_pseudo_ops = DecodeOptions { decode_pseudo_ops: false };

		let uarch_info = uarch_info_for_uarch(Uarch::HarvardMips161);

		let mut buffer: [u8;4] = [0; 4];

		for test_case in BASE_TEST_CASES.iter() {
			match test_case {
				&TestCase::Normal{instr, ref asm, delay, ..} => {
					buffer[0] = (instr >> 24) as u8;
					buffer[1] = (instr >> 16) as u8;
					buffer[2] = (instr >> 8)  as u8;
					buffer[3] = (instr >> 0)  as u8;

					assert_eq!(disasm(0, &buffer, uarch_info, &no_pseudo_ops),
					           Ok((asm.to_string(), 4, delay)));
				},

				&TestCase::Branch{addr, instr, ref asm, ..} => {
					buffer[0] = (instr >> 24) as u8;
					buffer[1] = (instr >> 16) as u8;
					buffer[2] = (instr >> 8)  as u8;
					buffer[3] = (instr >> 0)  as u8;

					assert_eq!(disasm(addr, &buffer, uarch_info, &no_pseudo_ops),
					           Ok((asm.to_string(), 4, true)));
				},
			}
		}
	}

	static PSEUDO_OP_TEST_CASES: [TestCase; 3] = [
		TestCase::Normal{ instr: 0x24020020, asm: "li      v0,32", delay: false, op: Op::RtI16(Mne::Li, Reg::Gpr(V0), 32) },

		TestCase::Normal{ instr: 0x00000000, asm: "nop",           delay: false, op: Op::Implied(Mne::Nop) },

		TestCase::Normal{ instr: 0x03A0F021, asm: "move    s8,sp", delay: false, op: Op::RtRs(Mne::Move, Reg::Gpr(S8), Reg::Gpr(SP)) },
	];

	#[test]
	fn decode_pseudo_ops() {
		let pseudo_ops = DecodeOptions { decode_pseudo_ops: true };

		let uarch_info = uarch_info_for_uarch(Uarch::HarvardMips161);

		for test_case in PSEUDO_OP_TEST_CASES.iter() {
			match test_case {
				&TestCase::Normal{instr, ref op, ..} => {
					assert_eq!(&decode(instr, 0, uarch_info, &pseudo_ops).unwrap(), op)
				},

				_ => {},
			}
		}
	}

	#[test]
	fn disasm_pseudo_ops() {
		let pseudo_ops = DecodeOptions { decode_pseudo_ops: true };

		let uarch_info = uarch_info_for_uarch(Uarch::HarvardMips161);

		let mut buffer: [u8;4] = [0; 4];

		for test_case in PSEUDO_OP_TEST_CASES.iter() {
			match test_case {
				&TestCase::Normal{instr, ref asm, delay, ..} => {
					buffer[0] = (instr >> 24) as u8;
					buffer[1] = (instr >> 16) as u8;
					buffer[2] = (instr >> 8)  as u8;
					buffer[3] = (instr >> 0)  as u8;

					assert_eq!(disasm(0, &buffer, uarch_info, &pseudo_ops),
					           Ok((asm.to_string(), 4, delay)));
				},

				_ => {},
			}
		}
	}
}

