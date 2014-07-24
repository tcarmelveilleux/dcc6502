/**********************************************************************************
 * dcc6502.c -> Main module of:                                                   *
 * Disassembler and Cycle Counter for the 6502 microprocessor                     *
 *                                                                                *
 * This code is offered under the MIT License (MIT)                               *
 *                                                                                *
 * Copyright (c) 2014 Tennessee Carmel-Veilleux <veilleux@tentech.ca              *
 *                                                                                *
 * Permission is hereby granted, free of charge, to any person obtaining a copy   *
 * of this software and associated documentation files (the "Software"), to deal  *
 * in the Software without restriction, including without limitation the rights   *
 * to use, copy, modify, merge, publish, distribute, sublicense, and/or sell      *
 * copies of the Software, and to permit persons to whom the Software is          *
 * furnished to do so, subject to the following conditions:                       *
 *                                                                                *
 * The above copyright notice and this permission notice shall be included in all *
 * copies or substantial portions of the Software.                                *
 *                                                                                *
 * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR     *
 * IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,       *
 * FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE    *
 * AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER         *
 * LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,  *
 * OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE  *
 * SOFTWARE.                                                                      *
 **********************************************************************************/
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <stdint.h>
#include <ctype.h>
#include <errno.h>

#define VERSION_INFO "v1.6"
#define NUMBER_OPCODES 151

/* The 6502's 13 addressing modes */
#define IMMED 0 /* Immediate */
#define ABSOL 1 /* Absolute */
#define ZEROP 2 /* Zero Page */
#define IMPLI 3 /* Implied */
#define INDIA 4 /* Indirect Absolute */
#define ABSIX 5 /* Absolute indexed with X */
#define ABSIY 6 /* Absolute indexed with Y */
#define ZEPIX 7 /* Zero page indexed with X */
#define ZEPIY 8 /* Zero page indexed with Y */
#define INDIN 9 /* Indexed indirect (with X) */
#define ININD 10 /* Indirect indexed (with Y) */
#define RELAT 11 /* Relative */
#define ACCUM 12 /* Accumulator */

/** Some compilers don't have EOK in errno.h */
#ifndef EOK
#define EOK 0
#endif

typedef struct OPcode {
    uint8_t number; /* Number of the opcode */
    unsigned int name; /* Index in the name table */
    // FIXME: Transform toe num
    unsigned int addressing; /* Addressing mode */
    unsigned int cycles; /* Number of cycles */
    unsigned int cross_page; /* 1 if cross-page boundaries affect cycles */
} OPcode;

typedef uint16_t word;

char name_table[56][4] = {
    "ADC", "AND", "ASL", "BCC", "BCS", "BEQ", "BIT", "BMI", "BNE", "BPL",
    "BRK", "BVC", "BVS", "CLC", "CLD", "CLI", "CLV", "CMP", "CPX", "CPY",
    "DEC", "DEX", "DEY", "EOR", "INC", "INX", "INY", "JMP", "JSR", "LDA",
    "LDX", "LDY", "LSR", "NOP", "ORA", "PHA", "PHP", "PLA", "PLP", "ROL",
    "ROR", "RTI", "RTS", "SBC", "SEC", "SED", "SEI", "STA", "STX", "STY",
    "TAX", "TAY", "TSX", "TXA", "TXS", "TYA"
};

/* Opcode table */
OPcode opcode_table[NUMBER_OPCODES] = {
    {0x69, 0, IMMED, 2, 1}, /* ADC */
    {0x65, 0, ZEROP, 3, 1},
    {0x75, 0, ZEPIX, 4, 1},
    {0x6D, 0, ABSOL, 4, 1},
    {0x7D, 0, ABSIX, 4, 1},
    {0x79, 0, ABSIY, 4, 1},
    {0x61, 0, INDIN, 6, 1},
    {0x71, 0, ININD, 5, 1},

    {0x29, 1, IMMED, 2, 1}, /* AND */
    {0x25, 1, ZEROP, 3, 1},
    {0x35, 1, ZEPIX, 4, 1},
    {0x2D, 1, ABSOL, 4, 1},
    {0x3D, 1, ABSIX, 4, 1},
    {0x39, 1, ABSIY, 4, 1},
    {0x21, 1, INDIN, 6, 1},
    {0x31, 1, ININD, 5, 1},

    {0x0A, 2, ACCUM, 2, 0}, /* ASL */
    {0x06, 2, ZEROP, 5, 0},
    {0x16, 2, ZEPIX, 6, 0},
    {0x0E, 2, ABSOL, 6, 0},
    {0x1E, 2, ABSIX, 6, 0},

    {0x90, 3, RELAT, 4, 1}, /* BCC */

    {0xB0, 4, RELAT, 4, 1}, /* BCS */

    {0xF0, 5, RELAT, 4, 1}, /* BEQ */

    {0x24, 6, ZEROP, 3, 0}, /* BIT */
    {0x2C, 6, ABSOL, 4, 0},

    {0x30, 7, RELAT, 4, 1}, /* BMI */

    {0xD0, 8, RELAT, 4, 1}, /* BNE */

    {0x10, 9, RELAT, 4, 1}, /* BPL */

    {0x00, 10, IMPLI, 7, 0}, /* BRK */

    {0x50, 11, RELAT, 4, 1}, /* BVC */

    {0x70, 12, RELAT, 4, 1}, /* BVS */

    {0x18, 13, IMPLI, 2, 0}, /* CLC */

    {0xD8, 14, IMPLI, 2, 0}, /* CLD */

    {0x58, 15, IMPLI, 2, 0}, /* CLI */

    {0xB8, 16, IMPLI, 2, 0}, /* CLV */

    {0xC9, 17, IMMED, 2, 0}, /* CMP */
    {0xC5, 17, ZEROP, 3, 0},
    {0xD5, 17, ZEPIX, 4, 0},
    {0xCD, 17, ABSOL, 4, 0},
    {0xDD, 17, ABSIX, 4, 0},
    {0xD9, 17, ABSIY, 4, 0},
    {0xC1, 17, INDIN, 6, 0},
    {0xD1, 17, ININD, 5, 0},

    {0xE0, 18, IMMED, 2, 0}, /* CPX */
    {0xE4, 18, ZEROP, 3, 0},
    {0xEC, 18, ABSOL, 4, 0},

    {0xC0, 19, IMMED, 2, 0}, /* CPY */
    {0xC4, 19, ZEROP, 3, 0},
    {0xCC, 19, ABSOL, 4, 0},

    {0xC6, 20, ZEROP, 5, 0}, /* DEC */
    {0xD6, 20, ZEPIX, 6, 0},
    {0xCE, 20, ABSOL, 6, 0},
    {0xDE, 20, ABSIX, 6, 0},

    {0xCA, 21, IMPLI, 2, 0}, /* DEX */

    {0x88, 22, IMPLI, 2, 0}, /* DEY */

    {0x49, 23, IMMED, 2, 1}, /* EOR */
    {0x45, 23, ZEROP, 3, 1},
    {0x55, 23, ZEPIX, 4, 1},
    {0x4D, 23, ABSOL, 4, 1},
    {0x5D, 23, ABSIX, 4, 1},
    {0x59, 23, ABSIY, 4, 1},
    {0x41, 23, INDIN, 6, 1},
    {0x51, 23, ININD, 5, 1},

    {0xE6, 24, ZEROP, 5, 0}, /* INC */
    {0xF6, 24, ZEPIX, 6, 0},
    {0xEE, 24, ABSOL, 6, 0},
    {0xFE, 24, ABSIX, 6, 0},

    {0xE8, 25, IMPLI, 2, 0}, /* INX */

    {0xC8, 26, IMPLI, 2, 0}, /* INY */

    {0x4C, 27, ABSOL, 3, 0}, /* JMP */
    {0x6C, 27, INDIA, 5, 0},

    {0x20, 28, ABSOL, 6, 0}, /* JSR */

    {0xA9, 29, IMMED, 2, 1}, /* LDA */
    {0xA5, 29, ZEROP, 3, 1},
    {0xB5, 29, ZEPIX, 4, 1},
    {0xAD, 29, ABSOL, 4, 1},
    {0xBD, 29, ABSIX, 4, 1},
    {0xB9, 29, ABSIY, 4, 1},
    {0xA1, 29, INDIN, 6, 1},
    {0xB1, 29, ININD, 5, 1},

    {0xA2, 30, IMMED, 2, 1}, /* LDX */
    {0xA6, 30, ZEROP, 3, 1},
    {0xB6, 30, ZEPIY, 4, 1},
    {0xAE, 30, ABSOL, 4, 1},
    {0xBE, 30, ABSIY, 4, 1},

    {0xA0, 31, IMMED, 2, 1}, /* LDY */
    {0xA4, 31, ZEROP, 3, 1},
    {0xB4, 31, ZEPIX, 4, 1},
    {0xAC, 31, ABSOL, 4, 1},
    {0xBC, 31, ABSIX, 4, 1},

    {0x4A, 32, ACCUM, 2, 0}, /* LSR */
    {0x46, 32, ZEROP, 5, 0},
    {0x56, 32, ZEPIX, 6, 0},
    {0x4E, 32, ABSOL, 6, 0},
    {0x5E, 32, ABSIX, 6, 0},

    {0xEA, 33, IMPLI, 2, 0}, /* NOP */

    {0x09, 34, IMMED, 2, 0}, /* ORA */
    {0x05, 34, ZEROP, 3, 0},
    {0x15, 34, ZEPIX, 4, 0},
    {0x0D, 34, ABSOL, 4, 0},
    {0x1D, 34, ABSIX, 4, 0},
    {0x19, 34, ABSIY, 4, 0},
    {0x01, 34, INDIN, 6, 0},
    {0x11, 34, ININD, 5, 0},

    {0x48, 35, IMPLI, 3, 0}, /* PHA */

    {0x08, 36, IMPLI, 3, 0}, /* PHP */

    {0x68, 37, IMPLI, 4, 0}, /* PLA */

    {0x28, 38, IMPLI, 4, 0}, /* PLP */

    {0x2A, 39, ACCUM, 2, 0}, /* ROL */
    {0x26, 39, ZEROP, 5, 0},
    {0x36, 39, ZEPIX, 6, 0},
    {0x2E, 39, ABSOL, 6, 0},
    {0x3E, 39, ABSIX, 6, 0},

    {0x6A, 40, ACCUM, 2, 0}, /* ROR */
    {0x66, 40, ZEROP, 5, 0},
    {0x76, 40, ZEPIX, 6, 0},
    {0x6E, 40, ABSOL, 6, 0},
    {0x7E, 40, ABSIX, 6, 0},

    {0x40, 41, IMPLI, 6, 0}, /* RTI */

    {0x60, 42, IMPLI, 6, 0}, /* RTS */

    {0xE9, 43, IMMED, 2, 1}, /* SBC */
    {0xE5, 43, ZEROP, 3, 1},
    {0xF5, 43, ZEPIX, 4, 1},
    {0xED, 43, ABSOL, 4, 1},
    {0xFD, 43, ABSIX, 4, 1},
    {0xF9, 43, ABSIY, 4, 1},
    {0xE1, 43, INDIN, 6, 1},
    {0xF1, 43, ININD, 5, 1},

    {0x38, 44, IMPLI, 2, 0}, /* SEC */

    {0xF8, 45, IMPLI, 2, 0}, /* SED */

    {0x78, 46, IMPLI, 2, 0}, /* SEI */

    {0x85, 47, ZEROP, 3, 0}, /* STA */
    {0x95, 47, ZEPIX, 4, 0},
    {0x8D, 47, ABSOL, 4, 0},
    {0x9D, 47, ABSIX, 4, 0},
    {0x99, 47, ABSIY, 4, 0},
    {0x81, 47, INDIN, 6, 0},
    {0x91, 47, ININD, 5, 0},

    {0x86, 48, ZEROP, 3, 0}, /* STX */
    {0x96, 48, ZEPIY, 4, 0},
    {0x8E, 48, ABSOL, 4, 0},

    {0x84, 49, ZEROP, 3, 0}, /* STY */
    {0x94, 49, ZEPIX, 4, 0},
    {0x8C, 49, ABSOL, 4, 0},

    {0xAA, 50, IMPLI, 2, 0}, /* TAX */

    {0xA8, 51, IMPLI, 2, 0}, /* TAY */

    {0xBA, 52, IMPLI, 2, 0}, /* TSX */

    {0x8A, 53, IMPLI, 2, 0}, /* TXA */

    {0x9A, 54, IMPLI, 2, 0}, /* TXS */

    {0x98, 55, IMPLI, 2, 0} /* TYA */
};

// FIXME: use g_ nomenclature for globals
uint16_t org; /* Origin of addresses */
int hex_output = 0; /* 1 if hex output is desired at beginning of line */
int cycle_counting = 0; /* 1 if we want cycle counting */
int nes_mode = 0; /* 1 if NES commenting and warnings are enabled */
FILE *f; /* Input file */
uint8_t buffer[0xffff]; /* Memory buffer */
uint16_t PC = 0; /* Program counter */
uint16_t max = 0xffff; /* Maximum number of bytes to disassemble */
char line[512];

/* This function emits a comment header with information about the file
   being disassembled */

void emit_header(char *filename, int fsize, uint16_t org) {
    fprintf(stdout, "; Source generated by DCC6502 version %s\n", VERSION_INFO);
    fprintf(stdout, "; For more info about DCC6502, see https://github.com/tcarmelveilleux/dcc6502\n");
    fprintf(stdout, "; FILENAME: %s, File Size: %d, ORG: $%04X\n", filename, fsize, org);
    if (hex_output) fprintf(stdout, ";     -> Hex output enabled\n");
    if (cycle_counting) fprintf(stdout, ";     -> Cycle counting enabled\n");
    if (nes_mode) fprintf(stdout, ";     -> NES mode enabled\n");
    fprintf(stdout, ";---------------------------------------------------------------------------\n");
}

/* This function appends cycle counting to the comment block */
char *append_cycle(char *input, uint8_t entry) {
    char tmpstr[256];
    int cycles = opcode_table[entry].cycles;

    // On page boundary crossing, instruction will take an extra cycle
    if (opcode_table[entry].cross_page) {
        sprintf(tmpstr, " Cycles: %d/%d", cycles, cycles + 1);
    } else {
        sprintf(tmpstr, " Cycles: %d", cycles);
    }

    strcat(input, tmpstr);
    return (input + strlen(input));
}

void add_nes_str(char *instr, char *instr2) {
    strcat(instr, " [NES] ");
    strcat(instr, instr2);
}

/* This function put NES-specific info in the comment block */
void append_nes(char *input, uint16_t arg) {
    switch(arg) {
        case 0x2000: add_nes_str(input, "PPU setup #1"); break;
        case 0x2001: add_nes_str(input, "PPU setup #2"); break;
        case 0x2002: add_nes_str(input, "PPU status"); break;
        case 0x2003: add_nes_str(input, "SPR-RAM address select"); break;
        case 0x2004: add_nes_str(input, "SPR-RAM data"); break;
        case 0x2005: add_nes_str(input, "PPU scroll"); break;
        case 0x2006: add_nes_str(input, "VRAM address select"); break;
        case 0x2007: add_nes_str(input, "VRAM data"); break;
        case 0x4000: add_nes_str(input, "Audio -> Square 1"); break;
        case 0x4001: add_nes_str(input, "Audio -> Square 1"); break;
        case 0x4002: add_nes_str(input, "Audio -> Square 1"); break;
        case 0x4003: add_nes_str(input, "Audio -> Square 1"); break;
        case 0x4004: add_nes_str(input, "Audio -> Square 2"); break;
        case 0x4005: add_nes_str(input, "Audio -> Square 2"); break;
        case 0x4006: add_nes_str(input, "Audio -> Square 2"); break;
        case 0x4007: add_nes_str(input, "Audio -> Square 2"); break;
        case 0x4008: add_nes_str(input, "Audio -> Triangle"); break;
        case 0x4009: add_nes_str(input, "Audio -> Triangle"); break;
        case 0x400a: add_nes_str(input, "Audio -> Triangle"); break;
        case 0x400b: add_nes_str(input, "Audio -> Triangle"); break;
        case 0x400c: add_nes_str(input, "Audio -> Noise control reg"); break;
        case 0x400e: add_nes_str(input, "Audio -> Noise Frequency reg #1"); break;
        case 0x400f: add_nes_str(input, "Audio -> Noise Frequency reg #2"); break;
        case 0x4010: add_nes_str(input, "Audio -> DPCM control"); break;
        case 0x4011: add_nes_str(input, "Audio -> DPCM D/A data"); break;
        case 0x4012: add_nes_str(input, "Audio -> DPCM address"); break;
        case 0x4013: add_nes_str(input, "Audio -> DPCM data length"); break;
        case 0x4014: add_nes_str(input, "Sprite DMA trigger"); break;
        case 0x4015: add_nes_str(input, "IRQ status / Sound enable"); break;
        case 0x4016: add_nes_str(input, "Joypad & I/O port for port #1"); break;
        case 0x4017: add_nes_str(input, "Joypad & I/O port for port #2"); break;
    }
}

#define DUMP_FORMAT (hex_output ? "%-16s%-16s;" : "%-8s%-16s;")
#define HIGH_PART(val) (((val) >> 8) & 0xFFu)
#define LOW_PART(val) ((val) & 0xFFu)
#define LOAD_WORD(buffer, current_pc) ((uint16_t)buffer[(current_pc) + 1] | (((uint16_t)buffer[(current_pc) + 2]) << 8))

/* This function disassembles the opcode at the PC and outputs it in *output */
void disassemble(char *output) {
    char opcode_repr[256], hex_dump[256];
    int i;
    int len = 0;
    int entry = 0;
    int found = 0;
    uint8_t tmp_byte1, opcode;
    word tmp_word = 0;
    uint16_t current_addr = org + PC;

    opcode = buffer[current_addr - org];
    opcode_repr[0] = '\0';
    hex_dump[0] = '\0';

    // Linear search for opcode
    for (i = 0; i < NUMBER_OPCODES; i++) {
        if (opcode == opcode_table[i].number) {
            /* Found the opcode, record its table index */
            found = 1;
            entry = i;
        }
    }

    // TODO: Normalize %02x versus %02X
    // For opcode not found, terminate early
    if (!found) {
        sprintf(opcode_repr, ".byte $%02x", opcode);
        if (hex_output) {
            sprintf(hex_dump, "$%04X> %02X:", current_addr, opcode);
            sprintf(output, "%-16s%-16s; INVALID OPCODE !!!\n", hex_dump, opcode_repr);
        } else {
            sprintf(hex_dump, "$%04X", current_addr);
            sprintf(output, "%-8s%-16s; INVALID OPCODE !!!\n", hex_dump, opcode_repr);
        }
        return;
    }

    // Opcode found in table: disassemble properly according to addressing mode

    // Set hex dump to default single address format. Will be overwritten
    // by more complex output in case of hex dump mode enabled
    sprintf(hex_dump, "$%04X", current_addr);

    switch (opcode_table[entry].addressing) {
        case IMMED:
            /* Get immediate value operand */
            tmp_byte1 = buffer[PC + 1];
            PC++;

            sprintf(opcode_repr, "%s #$%02x", name_table[opcode_table[entry].name], tmp_byte1);
            if (hex_output) {
                sprintf(hex_dump, "$%04X> %02X %02X:", current_addr, opcode, tmp_byte1);
            }

            break;
        case ABSOL:
            /* Get absolute address operand */
            tmp_word = LOAD_WORD(buffer, PC);
            PC += 2;

            sprintf(opcode_repr, "%s $%02X%02X", name_table[opcode_table[entry].name], HIGH_PART(tmp_word), LOW_PART(tmp_word));
            if (hex_output) {
                sprintf(hex_dump, "$%04X> %02X %02X%02X:", current_addr, opcode, LOW_PART(tmp_word), HIGH_PART(tmp_word));
            }

            break;
        case ZEROP:
            /* Get zero page address */
            tmp_byte1 = buffer[PC + 1];
            PC++;

            sprintf(opcode_repr, "%s $%02X", name_table[opcode_table[entry].name], tmp_byte1);
            if (hex_output) {
                sprintf(hex_dump, "$%04X> %02X %02X:", current_addr, opcode, tmp_byte1);
            }

            break;
        case IMPLI:
            sprintf(opcode_repr, "%s", name_table[opcode_table[entry].name]);
            if (hex_output) {
                sprintf(hex_dump, "$%04X> %02X:", current_addr, opcode);
            }

            break;
        case INDIA:
            /* Get indirection address */
            tmp_word = LOAD_WORD(buffer, PC);
            PC += 2;

            sprintf(opcode_repr, "%s ($%02X%02X)", name_table[opcode_table[entry].name], HIGH_PART(tmp_word), LOW_PART(tmp_word));
            if (hex_output) {
                sprintf(hex_dump, "$%04X> %02X %02X%02X:", current_addr, opcode, LOW_PART(tmp_word), HIGH_PART(tmp_word));
            }

            break;
        case ABSIX:
            /* Get base address */
            tmp_word = LOAD_WORD(buffer, PC);
            PC += 2;

            sprintf(opcode_repr, "%s $%02X%02X,X", name_table[opcode_table[entry].name], HIGH_PART(tmp_word), LOW_PART(tmp_word));
            if (hex_output) {
                sprintf(hex_dump, "$%04X> %02X %02X%02X:", current_addr, opcode, LOW_PART(tmp_word), HIGH_PART(tmp_word));
            }

            break;
        case ABSIY:
            /* Get baser address */
            tmp_word = LOAD_WORD(buffer, PC);
            PC += 2;

            sprintf(opcode_repr, "%s $%02X%02X,Y", name_table[opcode_table[entry].name], HIGH_PART(tmp_word), LOW_PART(tmp_word));
            if (hex_output) {
                sprintf(hex_dump, "$%04X> %02X %02X%02X:", current_addr, opcode, LOW_PART(tmp_word), HIGH_PART(tmp_word));
            }

            break;
        case ZEPIX:
            /* Get zero-page base address */
            tmp_byte1 = buffer[PC + 1];
            PC++;

            sprintf(opcode_repr, "%s $%02X,X", name_table[opcode_table[entry].name], tmp_byte1);
            if (hex_output) {
                sprintf(hex_dump, "$%04X> %02X %02X:", current_addr, opcode, tmp_byte1);
            }

            break;
        case ZEPIY:
            /* Get zero-page base address */
            tmp_byte1 = buffer[PC + 1];
            PC++;

            sprintf(opcode_repr, "%s $%02X,Y", name_table[opcode_table[entry].name], tmp_byte1);
            if (hex_output) {
                sprintf(hex_dump, "$%04X> %02X %02X:", current_addr, opcode, tmp_byte1);
            }

            break;
        case INDIN:
            /* Get zero-page base address */
            tmp_byte1 = buffer[PC + 1];
            PC++;

            sprintf(opcode_repr, "%s ($%02X,X)", name_table[opcode_table[entry].name], tmp_byte1);
            if (hex_output) {
                sprintf(hex_dump, "$%04X> %02X %02X:", current_addr, opcode, tmp_byte1);
            }

            break;
        case ININD:
            /* Get zero-page base address */
            tmp_byte1 = buffer[PC + 1];
            PC++;

            sprintf(opcode_repr, "%s ($%02X),Y", name_table[opcode_table[entry].name], tmp_byte1);
            if (hex_output) {
                sprintf(hex_dump, "$%04X> %02X %02X:", current_addr, opcode, tmp_byte1);
            }

            break;
        case RELAT:
            /* Get relative modifier */
            tmp_byte1 = buffer[PC + 1];
            PC++;

            // Compute displacement from first byte after full instruction.
            tmp_word = current_addr + 2;
            if (tmp_byte1 > 0x7Fu) {
                tmp_word -= ((~tmp_byte1 & 0x7Fu) + 1);
            } else {
                tmp_word += tmp_byte1 & 0x7Fu;
            }

            sprintf(opcode_repr, "%s $%04X", name_table[opcode_table[entry].name], tmp_word);
            if (hex_output) {
                sprintf(hex_dump, "$%04X> %02X %02X:", current_addr, opcode, tmp_byte1);
            }

            break;
        case ACCUM:
            sprintf(opcode_repr, "%s A", name_table[opcode_table[entry].name]);
            if (hex_output) {
                sprintf(hex_dump, "$%04X> %02X:", current_addr, opcode);
            }

            break;
        default:
            // Will not happen since each entry in opcode_table has address mode set
            break;
    }

    len = sprintf(output, DUMP_FORMAT, hex_dump, opcode_repr);
    output += len;

    /* Add cycle count if necessary */
    if (cycle_counting) {
        output = append_cycle(output, entry);
    }

    /* Add NES port info if necessary */
    switch (opcode_table[entry].addressing) {
        case ABSOL:
        case ABSIX:
        case ABSIY:
            if (nes_mode) {
                append_nes(output, tmp_word);
            }
            break;
        default:
            /* Other addressing modes: not enough info to add NES register annotation */
            break;
    }
}

void version(void) {
    fprintf(stderr, "DCC6502 %s (C)1998-2014 Tennessee Carmel-Veilleux <veilleux@tentech.ca>\n", VERSION_INFO);
    fprintf(stderr, "This software is licensed under the MIT license. See the LICENSE file.\n");
    fprintf(stderr, "See source on github: https://github.com/tcarmelveilleux/dcc6502.\n");
}

void usage_helper(char *str) {
    fprintf(stderr, "\t%s\n", str);
}

// FIXME: add command line sample
// FIXME: Make these more sane and add option for decimal
void usage(void) {
    usage_helper("-?      -> Show this help message");
    usage_helper("-oXXXX  -> Set the origin (ORG), where XXXX is hexadecimal [default: 8000]");
    usage_helper("-h      -> Enable hex dump within disassembly");
    usage_helper("-mXXXX  -> Only disassemble the first XXXX bytes, where XXXX is hexadecimal");
    usage_helper("-mXXXX  -> Only disassemble the first XXXX bytes, where XXXX is hexadecimal");
    usage_helper("-n      -> Enable NES register annotations");
    usage_helper("-v      -> Get only version information");
    usage_helper("-c      -> Enable cycle counting annotations");
    fprintf(stderr, "\n");
}

// FIXME: DE-KLUDGIFY THIS :D
uint16_t hex2int (char *str, uint16_t dfl) {
    uint32_t tmp = 0;

    errno = EOK;
    tmp = strtoul(str, NULL, 16);
    /* In case of conversion error, take default value */
    if (EOK != errno) {
        fprintf(stderr, "WARNING -> error converting %s to a numerical value.", str);
        return dfl;
    } else {
        return (uint16_t)(tmp & 0xFFFFu);
    }
}

void set_org(char *str) {
    if (strlen(str) < 3) {
        fprintf(stderr, "WARNING -> %s is not a valid ORG switch, defaulting to $8000\n", str);
        org = 0x8000;
        return;
    }

    org = hex2int(&str[2], 0x8000u);
}

void set_max(char *str) {
    if (strlen(str) < 3) {
        max = 0xFFFF-org;
        fprintf(stderr, "WARNING -> %s is not a valid MAX switch, defaulting to $%04X\n", str, max);
        return;
    }

    max = hex2int(&str[2], 0xFFFFu);
}

int main(int argc, char *argv[]) {
    int idx = 0;
    char tmpstring[512];
    char filename[512];

    cycle_counting = 0;
    hex_output = 0;
    org = 0x8000;

    if (argc < 2) {
        version();
        usage();
        exit(1);
    }

    if (argc > 2) {
        for (idx = 1; idx < argc - 1; idx++) {
            if (argv[idx][0] != '-') {
                version();
                usage();
                fprintf(stderr, "Unrecognized switch: %s\n", argv[idx]);
                exit(1);
            }
            switch (argv[idx][1]) {
                case '?':
                    version();
                    usage();
                    exit(0);
                    break;
                case 'n':
                    nes_mode = 1;
                    break;
                case 'c':
                    cycle_counting = 1;
                    break;
                case 'h':
                    hex_output = 1;
                    break;
                case 'v':
                    version();
                    exit(0);
                    break;
                case 'o':
                    set_org(argv[idx]);
                    break;
                case 'm':
                    set_max(argv[idx]);
                    break;
                default:
                    version();
                    usage();
                    fprintf(stderr, "Unrecognized switch: %s\n", argv[idx]);
                    exit(1);
            }
        }
    } else {
        if (argv[1][0] != '-') {
            strncpy(filename, argv[1], 511);
        } else {
            switch (argv[1][1]) {
                case '?':
                    version();
                    usage();
                    exit(0);
                    break;
                case 'v':
                    version();
                    exit(0);
                    break;
                default:
                    version();
                    usage();
                    fprintf(stderr, "Unrecognized switch: %s\n", argv[1]);
                    exit(1);

            }
        }
    }

    strncpy(filename, argv[argc - 1], 511);

    f = fopen(filename, "rb");

    if (NULL == f) {
        version();
        fprintf(stderr, "File not found or invalid filename : %s\n", filename);
        exit(1);
    }

    idx = 0;
    while(!feof(f) && ((idx + org) < 65535)) {
        fread(&buffer[idx], 1, 1, f);
        idx++;
    }

    fclose(f);

    emit_header(filename, idx, org);
    PC = 0;
    while(((PC + org) < 65535) && (PC <= max) && (PC < idx)) {
        disassemble(tmpstring);
        fprintf(stdout, "%s\n", tmpstring);
        PC++;
    }

    return 0;
}
