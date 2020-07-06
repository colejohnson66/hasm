/* ============================================================================
 * File:   mod.rs
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
mod detail;

use crate::io::iter::LineOffsetIterator;
pub use detail::*;
use std::io::Cursor;

pub struct AsmTokenizer {
    asm: LineOffsetIterator,
}

impl AsmTokenizer {
    pub fn new(asm: &str) -> AsmTokenizer {
        let mut cursor = Cursor::new(asm);
        AsmTokenizer {
            asm: LineOffsetIterator::new(&mut cursor),
        }
    }

    // In the impl blocks below, each fn MUST reset the state
    //   if nothing matches.
    // The return value is an Option<T> struct with two states:
    //  - None => no match
    //  - Some(T) => a match; T may or may not contain data
}

impl AsmTokenizer {
    pub fn read_line(&mut self) -> Option<Line> {
        // <Line> ::
        //     <AsmLine>
        //     <MacroLine>
        //     <DirectiveLine>
        //     epsilon

        let state = self.asm.state();

        // match <AsmLine>
        match self.asm_line() {
            Some(tok) => return Some(Line::Asm(tok)),
            None => (),
        }

        // match <MacroLine>
        match self.macro_line() {
            Some(tok) => return Some(Line::Macro(tok)),
            None => (),
        }

        // match <DirectiveLine>
        match self.directive_line() {
            Some(tok) => return Some(Line::Directive(tok)),
            None => (),
        }

        // don't reset state; return epsilon
        None
    }

    fn asm_line(&mut self) -> Option<AsmLine> {
        // <AsmLine> ::
        //     opt<Label> opt<Instruction> opt<Comment>

        let state = self.asm.state();

        // match opt<Label>
        let label = self.label();

        // match opt<Instruction>
        let instr = self.instruction();

        // match opt<Comment>
        let comment = self.comment();

        if label.is_none() && instr.is_none() && comment.is_none() {
            self.asm.set_state(state);
            return None;
        }

        Some(AsmLine {
            label,
            instr,
            comment,
        })
    }

    fn label(&mut self) -> Option<String> {
        // <Label> ::
        //     rep<LabelChar> ": "
        // <LabelChars> ::
        //     range['a'-'z']
        //     range['A'-'Z']
        //     range['0'-'9']
        //     oneOf['_', '$', '@']

        let state = self.asm.state();

        // match rep<LabelChar>
        let mut val: String = "".into();
        loop {
            match self.asm.peek() {
                Some(c) => {
                    if c.is_ascii_alphanumeric() {
                        self.asm.consume();
                        val.push(c);
                    } else if c == '_' || c == '$' || c == '@' {
                        self.asm.consume();
                        val.push(c);
                    } else {
                        break;
                    }
                }
                None => break,
            }
        }

        if val.len() == 0 {
            return None;
        }

        // match ": "
        match self.asm.read() {
            Some(':') => match self.asm.read() {
                Some(' ') => return Some(val),
                _ => (),
            },
            _ => (),
        }

        self.asm.set_state(state);
        None
    }

    fn instruction(&mut self) -> Option<Instruction> {
        // <Instruction> ::
        //     <Opcode>
        //     <Opcode> " " optrep[" "] <Operand>
        //     <Opcode> " " optrep[" "] <Operand> optrep[" "] "," optrep[" "] <Operand>
        //     <Opcode> " " optrep[" "] <Operand> optrep[" "] "," optrep[" "] <Operand> optrep[" "] "," optrep[" "] <Operand>
        //     <Opcode> " " optrep[" "] <Operand> optrep[" "] "," optrep[" "] <Operand> optrep[" "] "," optrep[" "] <Operand> optrep[" "] "," optrep[" "] <Operand>

        // TODO: allow directives (such as `db` too)

        let opcode = self.opcode();
        if opcode.is_none() {
            return None;
        }
        let opcode = opcode.unwrap();

        // match " "
        match self.asm.peek() {
            Some(' ') => self.asm.consume(),
            _ => {
                return Some(Instruction {
                    opcode,
                    operands: vec![],
                });
            }
        }

        let mut operands: Vec<OperandWithSeg> = Vec::with_capacity(8);

        // operand 1
        let state = self.asm.state();
        // match optrep[" "]
        self.asm.consume_all(' ');
        // match <Operand>
        match self.operand() {
            Some(operand) => operands.push(operand),
            None => {
                self.asm.set_state(state);
                return Some(Instruction { opcode, operands });
            }
        }

        // operand 2
        let state = self.asm.state();
        // match optrep[" "]
        self.asm.consume_all(' ');
        // match ","
        match self.asm.peek() {
            Some(',') => self.asm.consume(),
            _ => {
                self.asm.set_state(state);
                return Some(Instruction { opcode, operands });
            }
        }
        // match optrep[" "]
        self.asm.consume_all(' ');
        // match <Operand>
        match self.operand() {
            Some(operand) => operands.push(operand),
            None => {
                self.asm.set_state(state);
                return Some(Instruction { opcode, operands });
            }
        }

        // operand 3
        let state = self.asm.state();
        // match optrep[" "]
        self.asm.consume_all(' ');
        // match ","
        match self.asm.peek() {
            Some(',') => self.asm.consume(),
            _ => {
                self.asm.set_state(state);
                return Some(Instruction { opcode, operands });
            }
        }
        // match optrep[" "]
        self.asm.consume_all(' ');
        // match <Operand>
        match self.operand() {
            Some(operand) => operands.push(operand),
            None => {
                self.asm.set_state(state);
                return Some(Instruction { opcode, operands });
            }
        }

        // operand 4
        let state = self.asm.state();
        // match optrep[" "]
        self.asm.consume_all(' ');
        // match ","
        match self.asm.peek() {
            Some(',') => self.asm.consume(),
            _ => {
                self.asm.set_state(state);
                return Some(Instruction { opcode, operands });
            }
        }
        // match optrep[" "]
        self.asm.consume_all(' ');
        // match <Operand>
        match self.operand() {
            Some(operand) => operands.push(operand),
            None => {
                self.asm.set_state(state);
                return Some(Instruction { opcode, operands });
            }
        }

        Some(Instruction { opcode, operands })
    }

    fn opcode(&mut self) -> Option<String> {
        // <Opcode> ::
        //     rep<OpcodeChars>
        // <OpcodeChars> ::
        //     range['a'-'z']
        //     range['A'-'Z']
        //     range['0'-'9']
        // NOTE: <Opcode> is case insensitive; return all lowercase

        let state = self.asm.state();

        // match rep<LabelChar>
        let mut val: String = "".into();
        loop {
            match self.asm.peek() {
                Some(c) => {
                    if c.is_ascii_alphanumeric() {
                        self.asm.consume();
                        val.push(c.to_ascii_lowercase());
                    } else {
                        break;
                    }
                }
                None => break,
            }
        }

        if val.len() == 0 {
            return None;
        }

        Some(val)
    }

    fn operand(&mut self) -> Option<OperandWithSeg> {
        // <Operand> ::
        //     opt<SegmentPrefix> <Indirect>
        //     opt<SegmentPrefix> <Register>
        //     opt<SegmentPrefix> <Immediate>

        let state = self.asm.state();

        // match opt<SegmentPrefix>
        let seg = self.segment_prefix();

        // match <Indirect>
        match self.indirect() {
            Some(indirect) => {
                return Some(OperandWithSeg {
                    seg,
                    operand: Operand::Indirect(indirect),
                })
            }
            None => (),
        }

        // match <Register>
        match self.register() {
            Some(reg) => {
                return Some(OperandWithSeg {
                    seg,
                    operand: Operand::Reg(reg),
                })
            }
            None => (),
        }

        // match <Immediate>
        match self.immediate() {
            Some(imm) => {
                return Some(OperandWithSeg {
                    seg,
                    operand: Operand::Imm(imm),
                })
            }
            None => (),
        }

        self.asm.set_state(state);
        None
    }

    fn segment_prefix(&mut self) -> Option<Register> {
        // TODO: use <SegmentRegister>
        // <SegmentPrefix> ::
        //     <Segment> optrep[" "] ":" optrep[" "]
        // <Segment> ::
        //     caseInsensitive["cs"] // 0
        //     caseInsensitive["ds"] // 1
        //     caseInsensitive["ss"] // 2
        //     caseInsensitive["es"] // 3
        //     caseInsensitive["fs"] // 4
        //     caseInsensitive["gs"] // 5

        let state = self.asm.state();

        // match <Segment>
        let mut buf = ['\0'; 2];
        if self.asm.read_multiple(&mut buf[..]) != 2 {
            self.asm.set_state(state);
            return None;
        }
        let seg: Option<u8> = match &buf.iter().collect::<String>().to_ascii_lowercase()[..] {
            "cs" => Some(0),
            "ds" => Some(1),
            "ss" => Some(2),
            "es" => Some(3),
            "fs" => Some(4),
            "gs" => Some(5),
            _ => None,
        };
        if seg.is_none() {
            self.asm.set_state(state);
            return None;
        }
        let seg = RegisterType::Segment(seg.unwrap());

        // match optrep[" "]
        loop {
            match self.asm.read() {
                Some(' ') => continue,
                _ => break,
            }
        }

        // match ":"
        match self.asm.read() {
            Some(':') => (),
            _ => {
                self.asm.set_state(state);
                return None;
            }
        }

        // match optrep[" "]
        loop {
            match self.asm.read() {
                Some(' ') => continue,
                _ => break,
            }
        }

        Some(Register {
            bit_width: 0,
            reg: seg,
            flags: None,
            mask: None,
        })
    }

    fn indirect(&mut self) -> Option<Indirect> {
        // NOTE: <Indirect> takes many forms; Ultimately, it is an expression
        //   of the form  `[base+index*scale+disp]` where each value is
        //   optional, but at least one register must be present. In addition,
        //   optional whitespace may exist between each "block" (it is not
        //   present here for readability).
        // TODO: "[" <Register> "+" <Register> "+" <Number> "]" // Implied scale of 1
        // TODO: This is not exhaustive
        // <Indirect> ::
        //     "[" <Register> "]"
        //     "[" <Register> "+" <Register> "]"
        //     "[" <Register> "+" <Register> "*" <Scale> "]"
        //     "[" <Register> "+" <Register> "*" <Scale> "+" <Number> "]"
        // <Scale> ::
        //     oneOf["1", "2", "4", "8"]

        let state = self.asm.state();

        // match "["
        match self.asm.peek() {
            Some('[') => self.asm.consume(),
            _ => return None,
        }

        // match optrep[" "]
        self.asm.consume_all(' ');

        // match <Register>
        let base = self.register();
        if base.is_none() {
            self.asm.set_state(state);
            return None;
        }

        // match optrep[" "]
        self.asm.consume_all(' ');

        // match "]" or "+"
        match self.asm.peek() {
            Some(']') => {
                return Some(Indirect {
                    base,
                    index: None,
                    scale: 0,
                    disp: 0,
                })
            }
            Some('+') => (),
            _ => {
                self.asm.set_state(state);
                return None;
            }
        }

        // match optrep[" "]
        self.asm.consume_all(' ');

        // match <Register>
        let index = self.register();
        if index.is_none() {
            self.asm.set_state(state);
            return None;
        }

        // match optrep[" "]
        self.asm.consume_all(' ');

        // match "]" or "*"
        match self.asm.peek() {
            Some(']') => {
                return Some(Indirect {
                    base,
                    index,
                    scale: 1, // implied
                    disp: 0,
                });
            }
            Some('*') => (),
            _ => {
                self.asm.set_state(state);
                return None;
            }
        }

        // match optrep[" "]
        self.asm.consume_all(' ');

        // match <Scale>
        let scale = match self.asm.peek() {
            Some('1') => 1u8,
            Some('2') => 2u8,
            Some('4') => 4u8,
            Some('8') => 8u8,
            _ => {
                self.asm.set_state(state);
                return None;
            }
        };

        // match optrep[" "]
        self.asm.consume_all(' ');

        // match "]" or "+"
        match self.asm.peek() {
            Some(']') => {
                return Some(Indirect {
                    base,
                    index,
                    scale,
                    disp: 0,
                });
            }
            Some('+') => (),
            _ => {
                self.asm.set_state(state);
                return None;
            }
        }

        // match optrep[" "]
        self.asm.consume_all(' ');

        // match <Number>
        let disp = self.i32();
        if disp.is_none() {
            self.asm.set_state(state);
            return None;
        }
        let disp = disp.unwrap();

        // match optrep[" "]
        self.asm.consume_all(' ');

        // match "]"
        match self.asm.peek() {
            Some(']') => (),
            _ => {
                self.asm.set_state(state);
                return None;
            }
        }

        Some(Indirect {
            base,
            index,
            scale,
            disp,
        })
    }

    fn register(&mut self) -> Option<Register> {
        // <Register> ::
        //     <GeneralPurposeRegister>
        //     // TODO: rDX:rAX pair
        //     <SegmentRegister>
        //     <FlagsRegister>
        //     <FloatingPointRegister>
        //     <ControlRegister>
        //     <DebugRegister>
        //     <BoundRegister>
        //     <MmxRegister>
        //     <AvxRegister>
        //     <VSibRegister>

        // Don't need to save state as all fn will reset state upon failure

        // match <GeneralPurposeRegister>
        let val = self.general_purpose_register();
        if val.is_some() {
            return val;
        }

        // TODO: rDX:rAX pair (check before GPR)

        // match <SegmentRegister>
        let val = self.segment_register();
        if val.is_some() {
            return val;
        }
        // match <FlagsRegister>
        let val = self.flags_register();
        if val.is_some() {
            return val;
        }

        // match <FloatingPointRegister>
        let val = self.floating_point_register();
        if val.is_some() {
            return val;
        }

        // match <ControlRegister>
        let val = self.control_register();
        if val.is_some() {
            return val;
        }

        // match <DebugRegister>
        let val = self.debug_register();
        if val.is_some() {
            return val;
        }

        // match <BoundRegister>
        let val = self.bound_register();
        if val.is_some() {
            return val;
        }

        // match <MmxRegister>
        let val = self.mmx_register();
        if val.is_some() {
            return val;
        }

        // match <AvxRegister>
        let val = self.avx_register();
        if val.is_some() {
            return val;
        }

        // match <VSibRegister>
        let val = self.vsib_register();
        if val.is_some() {
            return val;
        }

        None
    }

    fn general_purpose_register(&mut self) -> Option<Register> {
        // All case insensitive; for clarity, `caseInsensitive[...]` is omitted
        // <GeneralPurposeRegister> ::
        //     oneOf["al", "ah", "ax", "eax", "rax"]
        //     oneOf["cl", "ch", "cx", "ecx", "rcx"]
        //     oneOf["dl", "dh", "dx", "edx", "rdx"]
        //     oneOf["bl", "bh", "bx", "ebx", "rbx"]
        //     oneOf["spl", "sph", "sp", "esp", "rsp"]
        //     oneOf["bpl", "bph", "bp", "ebp", "rbp"]
        //     oneOf["sil", "sih", "si", "esi", "rsi"]
        //     oneOf["dil", "dih", "di", "edi", "rdi"]
        //     oneOf["r8b", "r8w", "r8d", "r8"]
        //     oneOf["r9b", "r9w", "r9d", "r9"]
        //     oneOf["r10b", "r10w", "r10d", "r10"]
        //     oneOf["r11b", "r11w", "r11d", "r11"]
        //     oneOf["r12b", "r12w", "r12d", "r12"]
        //     oneOf["r13b", "r13w", "r13d", "r13"]
        //     oneOf["r14b", "r14w", "r14d", "r14"]
        //     oneOf["r15b", "r15w", "r15d", "r15"]

        let state = self.asm.state();

        // longest GPR name is 4 characters long
        let mut buf = ['\0'; 4];
        if self.asm.read_multiple(&mut buf) != 4 {
            // TODO: should we be failing here?
            self.asm.set_state(state);
            return None;
        }
        let buf = buf.iter().collect::<String>().to_ascii_lowercase();

        // match 4 character long ones first
        let reg: Option<(&'static str, u8)> = match &buf[..] {
            "r10b" => Some(("r10b", 8)),
            "r10w" => Some(("r10w", 16)),
            "r10d" => Some(("r10d", 32)),
            "r11b" => Some(("r11b", 8)),
            "r11w" => Some(("r11w", 16)),
            "r11d" => Some(("r11d", 32)),
            "r12b" => Some(("r12b", 8)),
            "r12w" => Some(("r12w", 16)),
            "r12d" => Some(("r12d", 32)),
            "r13b" => Some(("r13b", 8)),
            "r13w" => Some(("r13w", 16)),
            "r13d" => Some(("r13d", 32)),
            "r14b" => Some(("r14b", 8)),
            "r14w" => Some(("r14w", 16)),
            "r14d" => Some(("r14d", 32)),
            "r15b" => Some(("r15b", 8)),
            "r15w" => Some(("r15w", 16)),
            "r15d" => Some(("r15d", 32)),
            _ => None,
        };
        if reg.is_some() {
            let reg = reg.unwrap();
            return Some(Register {
                bit_width: reg.1.into(),
                reg: RegisterType::GeneralPurpose(reg.0.into()),
                flags: None,
                mask: None,
            });
        }

        // match 3 character long ones
        let reg: Option<(&'static str, u8)> = match &buf[..3] {
            "bph" => Some(("bph", 8)),
            "bpl" => Some(("bpl", 8)),
            "dih" => Some(("dih", 8)),
            "dil" => Some(("dil", 8)),
            "eax" => Some(("eax", 32)),
            "ebp" => Some(("ebp", 32)),
            "ebx" => Some(("ebx", 32)),
            "ecx" => Some(("ecx", 32)),
            "edi" => Some(("edi", 32)),
            "edx" => Some(("edx", 32)),
            "esi" => Some(("esi", 32)),
            "esp" => Some(("esp", 32)),
            "r8b" => Some(("r8b", 8)),
            "r8w" => Some(("r8w", 16)),
            "r8d" => Some(("r8d", 32)),
            "r9b" => Some(("r9b", 8)),
            "r9w" => Some(("r9w", 16)),
            "r9d" => Some(("r9d", 32)),
            "r10" => Some(("r10", 64)),
            "r11" => Some(("r11", 64)),
            "r12" => Some(("r12", 64)),
            "r13" => Some(("r13", 64)),
            "r14" => Some(("r14", 64)),
            "r15" => Some(("r15", 64)),
            "rax" => Some(("rax", 32)),
            "rbp" => Some(("rbp", 32)),
            "rbx" => Some(("rbx", 32)),
            "rcx" => Some(("rcx", 32)),
            "rdi" => Some(("rdi", 32)),
            "rdx" => Some(("rdx", 32)),
            "rsi" => Some(("rsi", 32)),
            "rsp" => Some(("rsp", 32)),
            "sih" => Some(("sih", 8)),
            "sil" => Some(("sil", 8)),
            "sph" => Some(("sph", 8)),
            "spl" => Some(("spl", 8)),
            _ => None,
        };
        if reg.is_some() {
            let reg = reg.unwrap();
            return Some(Register {
                bit_width: reg.1.into(),
                reg: RegisterType::GeneralPurpose(reg.0.into()),
                flags: None,
                mask: None,
            });
        }

        // finally, match 2 character long ones
        let reg: Option<(&'static str, u8)> = match &buf[..2] {
            "ah" => Some(("ah", 8)),
            "al" => Some(("al", 8)),
            "ax" => Some(("ax", 16)),
            "bh" => Some(("bh", 8)),
            "bl" => Some(("bl", 8)),
            "bp" => Some(("bp", 16)),
            "bx" => Some(("bx", 16)),
            "ch" => Some(("ch", 8)),
            "cl" => Some(("cl", 8)),
            "cx" => Some(("cx", 16)),
            "dh" => Some(("dh", 8)),
            "di" => Some(("di", 8)),
            "dl" => Some(("dl", 8)),
            "dx" => Some(("dx", 16)),
            "r8" => Some(("r8", 64)),
            "r9" => Some(("r9", 64)),
            "si" => Some(("si", 16)),
            "sp" => Some(("sp", 16)),
            _ => None,
        };
        if reg.is_some() {
            let reg = reg.unwrap();
            return Some(Register {
                bit_width: reg.1.into(),
                reg: RegisterType::GeneralPurpose(reg.0.into()),
                flags: None,
                mask: None,
            });
        }

        // no match; reset
        self.asm.set_state(state);
        None
    }

    fn segment_register(&mut self) -> Option<Register> {
        // <SegmentRegister> ::
        //     caseInsensitive["cs"]
        //     caseInsensitive["ds"]
        //     caseInsensitive["ss"]
        //     caseInsensitive["es"]
        //     caseInsensitive["fs"]
        //     caseInsensitive["gs"]

        let state = self.asm.state();

        let mut buf = ['\0'; 2];
        if self.asm.read_multiple(&mut buf) != 2 {
            self.asm.set_state(state);
            return None;
        }
        let buf = buf.iter().collect::<String>().to_ascii_lowercase();

        let seg: Option<u8> = match &buf[..] {
            "cs" => Some(0),
            "ds" => Some(1),
            "ss" => Some(2),
            "es" => Some(3),
            "fs" => Some(4),
            "gs" => Some(5),
            _ => None,
        };
        if seg.is_none() {
            self.asm.set_state(state);
            return None;
        }

        Some(Register {
            bit_width: 0,
            reg: RegisterType::Segment(seg.unwrap()),
            flags: None,
            mask: None,
        })
    }

    fn flags_register(&mut self) -> Option<Register> {
        // <FlagsRegister> ::
        //     caseInsensitive["flags"]
        //     caseInsensitive["eflags"]
        //     caseInsensitive["rflags"]

        let state = self.asm.state();

        let mut buf = ['\0'; 5];
        if self.asm.read_multiple(&mut buf[..]) != 5 {
            self.asm.set_state(state);
            return None;
        }
        let buf = buf.iter().collect::<String>().to_ascii_lowercase();
        let size: Option<u16> = match &buf[..] {
            "flags" => Some(16),
            "eflag" => match self.asm.read() {
                Some('s') => Some(32),
                _ => None,
            },
            "rflag" => match self.asm.read() {
                Some('s') => Some(64),
                _ => None,
            },
            _ => None,
        };
        if size.is_none() {
            self.asm.set_state(state);
            return None;
        }

        Some(Register {
            bit_width: size.unwrap(),
            reg: RegisterType::Flags,
            flags: None,
            mask: None,
        })
    }

    fn floating_point_register(&mut self) -> Option<Register> {
        // <FloatingPointRegister> ::
        //     caseInsensitive["st"] "(" range["0"-"7"] ")"
        //     caseInsensitive["st"] range["0"-"7"]

        let state = self.asm.state();

        let mut buf = ['\0'; 2];
        if self.asm.read_multiple(&mut buf) != 2 {
            self.asm.set_state(state);
            return None;
        }
        let buf = buf.iter().collect::<String>().to_ascii_lowercase();
        if &buf[..] != "st" {
            self.asm.set_state(state);
            return None;
        }

        // match "("
        let paren = match self.asm.peek() {
            Some('(') => {
                self.asm.consume();
                true
            }
            _ => false,
        };

        // match range["0"-"7"]
        let reg: u8 = match self.asm.read() {
            Some('0') => 0,
            Some('1') => 1,
            Some('2') => 2,
            Some('3') => 3,
            Some('4') => 4,
            Some('5') => 5,
            Some('6') => 6,
            Some('7') => 7,
            _ => {
                self.asm.set_state(state);
                return None;
            }
        };

        // match ")" if needed
        if paren {
            match self.asm.read() {
                Some(')') => (),
                _ => {
                    self.asm.set_state(state);
                    return None;
                }
            }
        }

        Some(Register {
            bit_width: 0,
            reg: RegisterType::FloatingPoint(reg),
            flags: None,
            mask: None,
        })
    }

    fn control_register(&mut self) -> Option<Register> {
        // <ControlRegister> ::
        //     caseInsensitive["cr"] range["0"-"15"]

        let state = self.asm.state();

        // match caseInsensitive["cr"]
        let mut buf = ['\0'; 2];
        if self.asm.read_multiple(&mut buf) != 2 {
            self.asm.set_state(state);
            return None;
        }
        let buf = buf.iter().collect::<String>().to_ascii_lowercase();
        if &buf[..] != "cr" {
            self.asm.set_state(state);
            return None;
        }

        // match range["0"-"15"]
        let reg_num = match self.asm.read() {
            Some('0') => Some(0),
            Some('1') => match self.asm.peek() {
                Some(c) if c >= '0' && c <= '5' => {
                    self.asm.read();
                    let digit = c as u8 - '0' as u8;
                    Some(10 + digit)
                }
                _ => Some(1),
            },
            Some(c) if c >= '2' && c <= '9' => {
                let digit = c as u8 - '0' as u8;
                Some(digit)
            }
            _ => None,
        };
        if reg_num.is_none() {
            self.asm.set_state(state);
            return None;
        }

        Some(Register {
            bit_width: 0,
            reg: RegisterType::Control(reg_num.unwrap()),
            flags: None,
            mask: None,
        })
    }

    fn debug_register(&mut self) -> Option<Register> {
        // <ControlRegister> ::
        //     caseInsensitive["dr"] range["0"-"15"]

        let state = self.asm.state();

        // match caseInsensitive["dr"]
        let mut buf = ['\0'; 2];
        if self.asm.read_multiple(&mut buf) != 2 {
            self.asm.set_state(state);
            return None;
        }
        let buf = buf.iter().collect::<String>().to_ascii_lowercase();
        if &buf[..] != "dr" {
            self.asm.set_state(state);
            return None;
        }

        // match range["0"-"15"]
        let reg_num = match self.asm.read() {
            Some('0') => Some(0),
            Some('1') => match self.asm.peek() {
                Some(c) if c >= '0' && c <= '5' => {
                    self.asm.read();
                    let digit = c as u8 - '0' as u8;
                    Some(10 + digit)
                }
                _ => Some(1),
            },
            Some(c) if c >= '2' && c <= '9' => {
                let digit = c as u8 - '0' as u8;
                Some(digit)
            }
            _ => None,
        };
        if reg_num.is_none() {
            self.asm.set_state(state);
            return None;
        }

        Some(Register {
            bit_width: 0,
            reg: RegisterType::Debug(reg_num.unwrap()),
            flags: None,
            mask: None,
        })
    }

    fn bound_register(&mut self) -> Option<Register> {
        // <BoundRegister> ::
        //     caseInsensitive["bnd"] range["0"-"3"]

        let state = self.asm.state();

        // match caseInsensitive["bnd"]
        let mut buf = ['\0'; 3];
        if self.asm.read_multiple(&mut buf) != 3 {
            self.asm.set_state(state);
            return None;
        }
        let buf = buf.iter().collect::<String>().to_ascii_lowercase();
        if &buf[..] != "bnd" {
            self.asm.set_state(state);
            return None;
        }

        // match range["0"-"3"]
        let reg_num: Option<u8> = match self.asm.read() {
            Some('0') => Some(0),
            Some('1') => Some(1),
            Some('2') => Some(2),
            Some('3') => Some(3),
            _ => None,
        };
        if reg_num.is_none() {
            self.asm.set_state(state);
            return None;
        }

        Some(Register {
            bit_width: 0,
            reg: RegisterType::Bound(reg_num.unwrap()),
            flags: None,
            mask: None,
        })
    }

    fn mmx_register(&mut self) -> Option<Register> {
        // <MmxRegister> ::
        //     caseInsensitive["mm"] range["0"-"7"]

        let state = self.asm.state();

        // match caseInsensitive["mm"]
        let mut buf = ['\0'; 2];
        if self.asm.read_multiple(&mut buf) != 2 {
            self.asm.set_state(state);
            return None;
        }
        let buf = buf.iter().collect::<String>().to_ascii_lowercase();
        if &buf[..] != "mm" {
            self.asm.set_state(state);
            return None;
        }

        // match range["0"-"3"]
        let reg_num: Option<u8> = match self.asm.read() {
            Some('0') => Some(0),
            Some('1') => Some(1),
            Some('2') => Some(2),
            Some('3') => Some(3),
            Some('4') => Some(4),
            Some('5') => Some(5),
            Some('6') => Some(6),
            Some('7') => Some(7),
            _ => None,
        };
        if reg_num.is_none() {
            self.asm.set_state(state);
            return None;
        }

        Some(Register {
            bit_width: 0,
            reg: RegisterType::Mmx(reg_num.unwrap()),
            flags: None,
            mask: None,
        })
    }

    fn avx_register(&mut self) -> Option<Register> {
        // <AvxRegister> ::
        //     caseInsensitive["xmm"] range["0"-"31"]
        //     caseInsensitive["ymm"] range["0"-"31"]
        //     caseInsensitive["zmm"] range["0"-"31"]

        let state = self.asm.state();

        // match caseInsensitive["xmm"]
        // match caseInsensitive["ymm"]
        // match caseInsensitive["zmm"]
        let mut buf = ['\0'; 3];
        if self.asm.read_multiple(&mut buf) != 3 {
            self.asm.set_state(state);
            return None;
        }
        let buf = buf.iter().collect::<String>().to_ascii_lowercase();
        let size: u16 = match &buf[..] {
            "xmm" => 128,
            "ymm" => 256,
            "zmm" => 512,
            _ => {
                self.asm.set_state(state);
                return None;
            }
        };

        // match range["0"-"31"]
        let reg_num = match self.asm.read() {
            Some('0') => 0,
            Some('1') => match self.asm.peek() {
                Some(c) if c >= '0' && c <= '9' => {
                    self.asm.read();
                    let digit = c as u8 - '0' as u8;
                    10 + digit
                }
                _ => 1,
            },
            Some('2') => match self.asm.peek() {
                Some(c) if c >= '0' && c <= '9' => {
                    self.asm.read();
                    let digit = c as u8 - '0' as u8;
                    10 + digit
                }
                _ => 2,
            },
            Some('3') => match self.asm.peek() {
                Some('0') => {
                    self.asm.read();
                    30
                }
                Some('1') => {
                    self.asm.read();
                    31
                }
                _ => 3,
            },
            Some('4') => 4,
            Some('5') => 5,
            Some('6') => 6,
            Some('7') => 7,
            Some('8') => 8,
            Some('9') => 9,
            _ => {
                self.asm.set_state(state);
                return None;
            }
        };

        Some(Register {
            bit_width: size,
            reg: RegisterType::Avx(reg_num),
            flags: None,
            mask: None,
        })
    }

    fn vsib_register(&mut self) -> Option<Register> {
        // <VsibRegister> ::
        //     caseInsensitive["vr"] range["0"-"15"]

        let state = self.asm.state();

        // match caseInsensitive["vr"]
        let mut buf = ['\0'; 2];
        if self.asm.read_multiple(&mut buf) != 2 {
            self.asm.set_state(state);
            return None;
        }
        let buf = buf.iter().collect::<String>().to_ascii_lowercase();
        if &buf[..] != "vr" {
            self.asm.set_state(state);
            return None;
        }

        // match range["0"-"15"]
        let reg_num = match self.asm.read() {
            Some('0') => Some(0),
            Some('1') => match self.asm.peek() {
                Some(c) if c >= '0' && c <= '5' => {
                    self.asm.read();
                    let digit = c as u8 - '0' as u8;
                    Some(10 + digit)
                }
                _ => Some(1),
            },
            Some(c) if c >= '2' && c <= '9' => {
                let digit = c as u8 - '0' as u8;
                Some(digit)
            }
            _ => None,
        };
        if reg_num.is_none() {
            self.asm.set_state(state);
            return None;
        }

        Some(Register {
            bit_width: 0,
            reg: RegisterType::VSib(reg_num.unwrap()),
            flags: None,
            mask: None,
        })
    }

    fn immediate(&mut self) -> Option<i64> {
        // <Immediate> ::
        //      <i64>

        self.i64()
    }

    fn comment(&mut self) -> Option<String> {
        None
        // unimplemented!();
    }

    fn macro_line(&mut self) -> Option<MacroLine> {
        None
        // unimplemented!();
    }

    fn directive_line(&mut self) -> Option<DirectiveLine> {
        None
        // unimplemented!();
    }

    fn i32(&mut self) -> Option<i32> {
        // <i32> ::
        //     range[-2147483648, 2147483647]
        //     caseInsensitive[range[-0x80000000, 0x7FFFFFFF]]

        let state = self.asm.state();

        let negative = match self.asm.peek() {
            Some('-') => true,
            Some(c) if c >= '0' && c <= '9' => false,
            _ => {
                //self.asm.set_state(state);
                return None;
            }
        };
        self.asm.consume();

        let is_hex = match self.asm.peek() {
            Some('x') | Some('X') => {
                self.asm.consume();
                true
            }
            _ => false,
        };

        let mut val: u128 = 0; // use u128 for headroom
        loop {
            if is_hex {
                match self.asm.peek() {
                    Some(c) if c >= '0' && c <= '9' => {
                        self.asm.consume();
                        val *= 16;
                        val += c as u128 - '0' as u128;
                    }
                    Some(c) if c >= 'a' && c <= 'f' => {
                        self.asm.consume();
                        val *= 16;
                        val += c as u128 - 'a' as u128;
                    }
                    Some(c) if c >= 'A' && c <= 'F' => {
                        self.asm.consume();
                        val *= 16;
                        val += c as u128 - 'A' as u128;
                    }
                    _ => break,
                }
            } else {
                match self.asm.peek() {
                    Some(c) if c >= '0' && c <= '9' => {
                        self.asm.consume();
                        val *= 10;
                        val += c as u128 - '0' as u128;
                    }
                    _ => break,
                }
            }
        }

        if negative && val > 0x80000000 {
            self.asm.set_state(state);
            return None;
        }
        if !negative && val > 0x7FFFFFFF {
            self.asm.set_state(state);
            return None;
        }

        let mut val: i64 = val as i64;
        if negative {
            val *= -1;
        }

        Some(val as i32)
    }

    fn i64(&mut self) -> Option<i64> {
        // <i64> ::
        //     range[-9223372036854775808, 9223372036854775807]
        //     caseInsensitive[range[-0x8000000000000000, 0x7FFFFFFFFFFFFFFF]]

        let state = self.asm.state();

        let negative = match self.asm.peek() {
            Some('-') => true,
            Some(c) if c >= '0' && c <= '9' => false,
            _ => {
                //self.asm.set_state(state);
                return None;
            }
        };
        self.asm.consume();

        let is_hex = match self.asm.peek() {
            Some('x') | Some('X') => {
                self.asm.consume();
                true
            }
            _ => false,
        };

        let mut val: u128 = 0; // use u128 for headroom
        loop {
            if is_hex {
                match self.asm.peek() {
                    Some(c) if c >= '0' && c <= '9' => {
                        self.asm.consume();
                        val *= 16;
                        val += c as u128 - '0' as u128;
                    }
                    Some(c) if c >= 'a' && c <= 'f' => {
                        self.asm.consume();
                        val *= 16;
                        val += c as u128 - 'a' as u128;
                    }
                    Some(c) if c >= 'A' && c <= 'F' => {
                        self.asm.consume();
                        val *= 16;
                        val += c as u128 - 'A' as u128;
                    }
                    _ => break,
                }
            } else {
                match self.asm.peek() {
                    Some(c) if c >= '0' && c <= '9' => {
                        self.asm.consume();
                        val *= 10;
                        val += c as u128 - '0' as u128;
                    }
                    _ => break,
                }
            }
        }

        if negative && val > 0x8000000000000000 {
            self.asm.set_state(state);
            return None;
        }
        if !negative && val > 0x7FFFFFFFFFFFFFFF {
            self.asm.set_state(state);
            return None;
        }

        let mut val: i128 = val as i128;
        if negative {
            val *= -1;
        }

        Some(val as i64)
    }
}
