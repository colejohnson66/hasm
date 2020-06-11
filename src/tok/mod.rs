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
    fn read_line(&mut self) -> Option<Line> {
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
        unimplemented!();
    }

    fn directive(&mut self) -> Option<Directive> {
        unimplemented!();
    }

    fn comment(&mut self) -> Option<String> {
        unimplemented!();
    }

    fn macro_line(&mut self) -> Option<MacroLine> {
        unimplemented!();
    }

    fn directive_line(&mut self) -> Option<DirectiveLine> {
        unimplemented!();
    }
}
