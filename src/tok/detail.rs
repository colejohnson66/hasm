/* ============================================================================
 * File:   detail.rs
 * Author: Cole Johnson
 * ============================================================================
 * Copyright (c) 2020 Cole Johnson
 *
 * This file is part of hasm.
 *
 * hasm is free software: you can redistribute it and/or modify it under the
 *   terms of the GNU General Public License as published by the Free Software
 *   Foundation, either version 3 of the License, or (at your option) any later
 *   version.
 *
 * hasm is distributed in the hope that it will be useful, but WITHOUT ANY
 *   WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
 *   FOR A PARTICULAR PURPOSE. See the GNU General Public License for more
 *   details.
 *
 * You should have received a copy of the GNU General Public License along with
 *   hasm. If not, see <http://www.gnu.org/licenses/>.
 * ============================================================================
 */
#[derive(Debug)]
pub struct Instruction {
    pub opcode: String,
    pub operands: Vec<OperandWithSeg>,
}

#[derive(Debug)]
pub struct OperandWithSeg {
    pub seg: Option<Register>,
    pub operand: Operand,
}

#[derive(Debug)]
pub enum Operand {
    Indirect(Indirect),
    Reg(Register),
    Imm(i64),
}

// [base + index*scale + disp]
#[derive(Debug)]
pub struct Indirect {
    pub base: Option<Register>,
    pub index: Option<Register>,
    pub scale: u8,
    pub disp: i32, // TODO: i64?
}

#[derive(Debug)]
pub struct Register {
    pub bit_width: u16, // set to 0 if size is irrelevant (such as with segments)
    pub reg: RegisterType,
    pub flags: Option<AvxFlags>,
    // k0..7; {z}
    pub mask: Option<(u8, bool)>,
}

#[derive(Debug)]
pub enum AvxFlags {
    EmbeddedRounding,      // {er} (implies {sae})
    SuppressAllExceptions, // {sae}
}

#[derive(Debug)]
pub enum RegisterType {
    /* In order (note rbx is NOT idx 1)
     * 0 - rax: accumulator
     * 1 - rcx: counter
     * 2 - rdx: data
     * 3 - rbx: base
     * 4 - rsp: stack pointer
     * 5 - rbp: base pointer
     * 6 - rsi: source index
     * 7 - rdi: destination index
     * 8 - r8-r15: x86-64 extended registers
     *
     * NOTE: In long mode, the order for
     *   8-bit is (based on REX):
     *     w/    w/o
     * 0 - al  | al
     * 1 - cl  | cl
     * 2 - dl  | dl
     * 3 - bl  | bl
     * 4 - spl | ah
     * 5 - bpl | ch
     * 6 - sil | dh
     * 7 - dil | bh
     */
    GeneralPurpose(String),
    // rdx:rax
    RdxRaxPair,
    /* In order: (TODO: correct?)
     * 0 - cs: code
     * 1 - ds: data
     * 2 - ss: stack
     * 3 - es: data
     * 4 - fs: data
     * 5 - gs: data
     */
    Segment(u8),
    // RFLAGS
    Flags,
    // ST0..7
    FloatingPoint(u8),
    // CR0..15
    Control(u8),
    // DR0..15
    Debug(u8),
    // BND0..3
    Bound(u8),
    // MM0..7
    Mmx(u8),
    // XMM0..31
    // YMM0..31
    // ZMM0..31
    Avx(u8),
    // VR0..15
    VSib(u8),
    // TODO: BNDCFGU and BNDSTATUS?
    // TODO: XCR0?
    // TODO: CET registers?
}

#[derive(Debug)]
pub struct Directive {
    pub directive: String,
    // TODO: values
}

#[derive(Debug)]
pub struct AsmLine {
    pub label: Option<String>,
    pub instr: Option<Instruction>,
    pub comment: Option<String>,
}

#[derive(Debug)]
pub struct MacroLine {}

#[derive(Debug)]
pub struct DirectiveLine {
    pub directive: String,
    pub args: Vec<String>,
}

#[derive(Debug)]
pub enum Line {
    Asm(AsmLine),
    Macro(MacroLine),
    Directive(DirectiveLine),
}
