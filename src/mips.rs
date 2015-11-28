use super::Addr;

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

#[allow(unused_variables)]
pub fn decode_instr(instr: u32, addr: Addr, uarch_info: &UarchInfo) {
}

