/* -*- mode: D -*-
 *
 * Copyright (c) 2007-2011, 2013, 2014 The University of Utah
 * All rights reserved.
 *
 * This file is part of `csmith', a random generator of C programs.
 *
 * Redistribution and use in source and binary forms, with or without
 * modification, are permitted provided that the following conditions are met:
 *
 *   * Redistributions of source code must retain the above copyright notice,
 *     this list of conditions and the following disclaimer.
 *
 *   * Redistributions in binary form must reproduce the above copyright
 *     notice, this list of conditions and the following disclaimer in the
 *     documentation and/or other materials provided with the distribution.
 *
 * THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS"
 * AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
 * IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
 * ARE DISCLAIMED.  IN NO EVENT SHALL THE COPYRIGHT OWNER OR CONTRIBUTORS BE
 * LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR
 * CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF
 * SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS
 * INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN
 * CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE)
 * ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE
 * POSSIBILITY OF SUCH DAMAGE.
 */
module csmith;

alias uint8_t = ubyte;
alias uint16_t = ushort;
alias uint32_t = uint;
alias uint64_t = ulong;

alias int8_t = byte;
alias int16_t = short;
alias int32_t = int;
alias int64_t = long;

static uint32_t crc32_tab[256];
static uint32_t crc32_context = 0xFFFFFFFFUL;

void crc32_gentab()
{
	uint32_t crc;
	const uint32_t poly = 0xEDB88320UL;
	int i, j;
	
	for (i = 0; i < 256; i++) {
		crc = i;
		for (j = 8; j > 0; j--) {
			if (crc & 1) {
				crc = (crc >> 1) ^ poly;
			} else {
				crc >>= 1;
			}
		}
		crc32_tab[i] = crc;
	}
}

void crc32_byte (uint8_t b) {
	crc32_context = 
		((crc32_context >> 8) & 0x00FFFFFF) ^ 
		crc32_tab[(crc32_context ^ b) & 0xFF];
}


void crc32_8bytes (uint64_t val)
{
	crc32_byte ((val>>0) & 0xff);
	crc32_byte ((val>>8) & 0xff);
	crc32_byte ((val>>16) & 0xff);
	crc32_byte ((val>>24) & 0xff);
	crc32_byte ((val>>32) & 0xff);
	crc32_byte ((val>>40) & 0xff);
	crc32_byte ((val>>48) & 0xff);
	crc32_byte ((val>>56) & 0xff);
}

extern(C) int putchar(int ch);

void transparent_crc (uint64_t val,  const(char)[] vname, int flag)
{
	crc32_8bytes(val);
	if (flag) {
  		ulong x = crc32_context ^ 0xFFFFFFFFUL;
		my_puts ("...checksum after hashing ");
		my_puts(vname);
		putchar(' ');
	    for (int i=0; i<16; i++) {
	      put_hex (x & 0xf);
	      x >>= 4;
	    }
	    putchar ('\n');
	}
}


void transparent_crc_bytes (char *ptr, int nbytes, const(char)[] vname, int flag)
{
    int i;
    for (i=0; i<nbytes; i++) {
        crc32_byte(ptr[i]);
    }
	if (flag) {
		ulong x = crc32_context ^ 0xFFFFFFFFUL;
		my_puts ("...checksum after hashing ");
		my_puts(vname);
		putchar(' ');
	    for (i=0; i<16; i++) {
	      put_hex (x & 0xf);
	      x >>= 4;
	    }
	    putchar ('\n');
	}
}

void my_puts (const(char)[] p)
{
  foreach (char c; p)
    putchar (c);
}

void put_hex (int x)
{
  switch (x) {
  case 0x0: putchar ('0'); break;
  case 0x1: putchar ('1'); break;
  case 0x2: putchar ('2'); break;
  case 0x3: putchar ('3'); break;
  case 0x4: putchar ('4'); break;
  case 0x5: putchar ('5'); break;
  case 0x6: putchar ('6'); break;
  case 0x7: putchar ('7'); break;
  case 0x8: putchar ('8'); break;
  case 0x9: putchar ('9'); break;
  case 0xa: putchar ('a'); break;
  case 0xb: putchar ('b'); break;
  case 0xc: putchar ('c'); break;
  case 0xd: putchar ('d'); break;
  case 0xe: putchar ('e'); break;
  case 0xf: putchar ('f'); break;
  default: break;
  }
}

void platform_main_begin(){}

void platform_main_end (uint64_t x, int flag)
{
  if (!flag) {
    int i;
    my_puts ("checksum = ");
    for (i=0; i<16; i++) {
      put_hex (x & 0xf);
      x >>= 4;
    }
    putchar ('\n');
  }
}

/*


#define safe_unary_minus_func_int8_t_s(si,_si) \
  ((int8_t)( si = (_si), \
   (((int8_t)(si))==(INT8_MIN))? \
    ((int8_t)(si)): \
    (-((int8_t)(si))) \
  ))

#define safe_add_func_int8_t_s_s(si1,_si1,si2,_si2) \
        ((int8_t)( si1 = (_si1), si2 = (_si2) , \
                 (((((int8_t)(si1))>((int8_t)0)) && (((int8_t)(si2))>((int8_t)0)) && (((int8_t)(si1)) > ((INT8_MAX)-((int8_t)(si2))))) \
          || ((((int8_t)(si1))<((int8_t)0)) && (((int8_t)(si2))<((int8_t)0)) && (((int8_t)(si1)) < ((INT8_MIN)-((int8_t)(si2)))))) ? \
         ((int8_t)(si1)) :                      \
         (((int8_t)(si1)) + ((int8_t)(si2)))                \
        )) 

#define safe_sub_func_int8_t_s_s(si1,_si1,si2,_si2) \
        ((int8_t)( si1 = (_si1), si2 = (_si2) , \
                (((((int8_t)(si1))^((int8_t)(si2))) \
        & (((((int8_t)(si1)) ^ ((((int8_t)(si1))^((int8_t)(si2))) \
        & (((int8_t)1) << (sizeof(int8_t)*CHAR_BIT-1))))-((int8_t)(si2)))^((int8_t)(si2)))) < ((int8_t)0)) \
        ? ((int8_t)(si1)) \
        : (((int8_t)(si1)) - ((int8_t)(si2))) \
        ))

#define safe_mul_func_int8_t_s_s(si1,_si1,si2,_si2) \
  ((int8_t)( si1 = (_si1), si2 = (_si2) , \
  (((((int8_t)(si1)) > ((int8_t)0)) && (((int8_t)(si2)) > ((int8_t)0)) && (((int8_t)(si1)) > ((INT8_MAX) / ((int8_t)(si2))))) || \
  ((((int8_t)(si1)) > ((int8_t)0)) && (((int8_t)(si2)) <= ((int8_t)0)) && (((int8_t)(si2)) < ((INT8_MIN) / ((int8_t)(si1))))) || \
  ((((int8_t)(si1)) <= ((int8_t)0)) && (((int8_t)(si2)) > ((int8_t)0)) && (((int8_t)(si1)) < ((INT8_MIN) / ((int8_t)(si2))))) || \
  ((((int8_t)(si1)) <= ((int8_t)0)) && (((int8_t)(si2)) <= ((int8_t)0)) && (((int8_t)(si1)) != ((int8_t)0)) && (((int8_t)(si2)) < ((INT8_MAX) / ((int8_t)(si1)))))) \
  ? ((int8_t)(si1)) \
  : ((int8_t)(si1)) * ((int8_t)(si2))))

#define safe_mod_func_int8_t_s_s(si1,_si1,si2,_si2) \
  ((int8_t)( si1 = (_si1), si2 = (_si2) , \
  ((((int8_t)(si2)) == ((int8_t)0)) || ((((int8_t)(si1)) == (INT8_MIN)) && (((int8_t)(si2)) == ((int8_t)-1)))) \
  ? ((int8_t)(si1)) \
  : (((int8_t)(si1)) % ((int8_t)(si2)))))

#define safe_div_func_int8_t_s_s(si1,_si1,si2,_si2) \
  ((int8_t)( si1 = (_si1), si2 = (_si2) , \
  ((((int8_t)(si2)) == ((int8_t)0)) || ((((int8_t)(si1)) == (INT8_MIN)) && (((int8_t)(si2)) == ((int8_t)-1)))) \
  ? ((int8_t)(si1)) \
  : (((int8_t)(si1)) / ((int8_t)(si2)))))

#define c99_strict_safe_lshift_func_int8_t_s_s(left,_left,right,_right) \
  ((int8_t)( left = (_left), right = (_right) , \
   ( \
   (((int8_t)(left)) < ((int8_t)0)) \
  || (((int)(right)) < ((int8_t)0)) \
  || (((int)(right)) >= sizeof(int8_t)*CHAR_BIT) \
  || (((int8_t)(left)) > ((INT8_MAX) >> ((int)(right))))) \
  ? ((int8_t)(left)) \
  : (((int8_t)(left)) << ((int)(right)))))

#define safe_lshift_func_int8_t_s_s(left,_left,right,_right) \
  ((int8_t)( left = (_left), right = (_right) , \
   ( \
    (((int)(right)) < ((int8_t)0)) \
  || (((int)(right)) >= sizeof(int8_t)*CHAR_BIT) \
   ) \
  ? ((int8_t)(left)) \
  : (((int8_t)(left)) << ((int)(right)))))

#define c99_strict_safe_lshift_func_int8_t_s_u(left,_left,right,_right) \
  ((int8_t)( left = (_left), right = (_right) , \
   ((((int8_t)(left)) < ((int8_t)0)) \
  || (((unsigned int)(right)) >= sizeof(int8_t)*CHAR_BIT) \
  || (((int8_t)(left)) > ((INT8_MAX) >> ((unsigned int)(right))))) \
  ? ((int8_t)(left)) \
  : (((int8_t)(left)) << ((unsigned int)(right)))))

#define safe_lshift_func_int8_t_s_u(left,_left,right,_right) \
  ((int8_t)( left = (_left), right = (_right) , \
   ( \
   (((unsigned int)(right)) >= sizeof(int8_t)*CHAR_BIT) \
   ) \
  ? ((int8_t)(left)) \
  : (((int8_t)(left)) << ((unsigned int)(right)))))

#define c99_strict_safe_rshift_func_int8_t_s_s(left,_left,right,_right) \
    ((int8_t)( left = (_left), right = (_right) , \
        ((((int8_t)(left)) < ((int8_t)0)) \
             || (((int)(right)) < ((int8_t)0)) \
             || (((int)(right)) >= sizeof(int8_t)*CHAR_BIT)) \
            ? ((int8_t)(left)) \
            : (((int8_t)(left)) >> ((int)(right)))))

#define c99_strict_safe_rshift_func_int8_t_s_u(left,_left,right,_right) \
  ((int8_t)( left = (_left), right = (_right) , \
   ((((int8_t)(left)) < ((int8_t)0)) \
             || (((unsigned int)(right)) >= sizeof(int8_t)*CHAR_BIT)) \
            ? ((int8_t)(left)) \
            : (((int8_t)(left)) >> ((unsigned int)(right)))))

#define safe_rshift_func_int8_t_s_s(left,_left,right,_right) \
    ((int8_t)( left = (_left), right = (_right) , \
        ( \
              (((int)(right)) < ((int8_t)0)) \
             || (((int)(right)) >= sizeof(int8_t)*CHAR_BIT)) \
            ? ((int8_t)(left)) \
            : (((int8_t)(left)) >> ((int)(right)))))

#define safe_rshift_func_int8_t_s_u(left,_left,right,_right) \
  ((int8_t)( left = (_left), right = (_right) , \
   ( \
    (((unsigned int)(right)) >= sizeof(int8_t)*CHAR_BIT)) \
    ? ((int8_t)(left)) \
    : (((int8_t)(left)) >> ((unsigned int)(right)))))



#define safe_unary_minus_func_int16_t_s(si,_si) \
  ((int16_t)( si = (_si), \
   (((int16_t)(si))==(INT16_MIN))? \
    ((int16_t)(si)): \
    (-((int16_t)(si))) \
  ))

#define safe_add_func_int16_t_s_s(si1,_si1,si2,_si2) \
        ((int16_t)( si1 = (_si1), si2 = (_si2) , \
                 (((((int16_t)(si1))>((int16_t)0)) && (((int16_t)(si2))>((int16_t)0)) && (((int16_t)(si1)) > ((INT16_MAX)-((int16_t)(si2))))) \
          || ((((int16_t)(si1))<((int16_t)0)) && (((int16_t)(si2))<((int16_t)0)) && (((int16_t)(si1)) < ((INT16_MIN)-((int16_t)(si2)))))) ? \
         ((int16_t)(si1)) :                     \
         (((int16_t)(si1)) + ((int16_t)(si2)))              \
        )) 

#define safe_sub_func_int16_t_s_s(si1,_si1,si2,_si2) \
        ((int16_t)( si1 = (_si1), si2 = (_si2) , \
                (((((int16_t)(si1))^((int16_t)(si2))) \
        & (((((int16_t)(si1)) ^ ((((int16_t)(si1))^((int16_t)(si2))) \
        & (((int16_t)1) << (sizeof(int16_t)*CHAR_BIT-1))))-((int16_t)(si2)))^((int16_t)(si2)))) < ((int16_t)0)) \
        ? ((int16_t)(si1)) \
        : (((int16_t)(si1)) - ((int16_t)(si2))) \
        ))

#define safe_mul_func_int16_t_s_s(si1,_si1,si2,_si2) \
  ((int16_t)( si1 = (_si1), si2 = (_si2) , \
  (((((int16_t)(si1)) > ((int16_t)0)) && (((int16_t)(si2)) > ((int16_t)0)) && (((int16_t)(si1)) > ((INT16_MAX) / ((int16_t)(si2))))) || \
  ((((int16_t)(si1)) > ((int16_t)0)) && (((int16_t)(si2)) <= ((int16_t)0)) && (((int16_t)(si2)) < ((INT16_MIN) / ((int16_t)(si1))))) || \
  ((((int16_t)(si1)) <= ((int16_t)0)) && (((int16_t)(si2)) > ((int16_t)0)) && (((int16_t)(si1)) < ((INT16_MIN) / ((int16_t)(si2))))) || \
  ((((int16_t)(si1)) <= ((int16_t)0)) && (((int16_t)(si2)) <= ((int16_t)0)) && (((int16_t)(si1)) != ((int16_t)0)) && (((int16_t)(si2)) < ((INT16_MAX) / ((int16_t)(si1)))))) \
  ? ((int16_t)(si1)) \
  : ((int16_t)(si1)) * ((int16_t)(si2))))

#define safe_mod_func_int16_t_s_s(si1,_si1,si2,_si2) \
  ((int16_t)( si1 = (_si1), si2 = (_si2) , \
  ((((int16_t)(si2)) == ((int16_t)0)) || ((((int16_t)(si1)) == (INT16_MIN)) && (((int16_t)(si2)) == ((int16_t)-1)))) \
  ? ((int16_t)(si1)) \
  : (((int16_t)(si1)) % ((int16_t)(si2)))))

#define safe_div_func_int16_t_s_s(si1,_si1,si2,_si2) \
  ((int16_t)( si1 = (_si1), si2 = (_si2) , \
  ((((int16_t)(si2)) == ((int16_t)0)) || ((((int16_t)(si1)) == (INT16_MIN)) && (((int16_t)(si2)) == ((int16_t)-1)))) \
  ? ((int16_t)(si1)) \
  : (((int16_t)(si1)) / ((int16_t)(si2)))))

#define c99_strict_safe_lshift_func_int16_t_s_s(left,_left,right,_right) \
  ((int16_t)( left = (_left), right = (_right) , \
   ( \
   (((int16_t)(left)) < ((int16_t)0)) \
  || (((int)(right)) < ((int16_t)0)) \
  || (((int)(right)) >= sizeof(int16_t)*CHAR_BIT) \
  || (((int16_t)(left)) > ((INT16_MAX) >> ((int)(right))))) \
  ? ((int16_t)(left)) \
  : (((int16_t)(left)) << ((int)(right)))))

#define safe_lshift_func_int16_t_s_s(left,_left,right,_right) \
  ((int16_t)( left = (_left), right = (_right) , \
   ( \
    (((int)(right)) < ((int16_t)0)) \
  || (((int)(right)) >= sizeof(int16_t)*CHAR_BIT) \
   ) \
  ? ((int16_t)(left)) \
  : (((int16_t)(left)) << ((int)(right)))))

#define c99_strict_safe_lshift_func_int16_t_s_u(left,_left,right,_right) \
  ((int16_t)( left = (_left), right = (_right) , \
   ((((int16_t)(left)) < ((int16_t)0)) \
  || (((unsigned int)(right)) >= sizeof(int16_t)*CHAR_BIT) \
  || (((int16_t)(left)) > ((INT16_MAX) >> ((unsigned int)(right))))) \
  ? ((int16_t)(left)) \
  : (((int16_t)(left)) << ((unsigned int)(right)))))

#define safe_lshift_func_int16_t_s_u(left,_left,right,_right) \
  ((int16_t)( left = (_left), right = (_right) , \
   ( \
   (((unsigned int)(right)) >= sizeof(int16_t)*CHAR_BIT) \
   ) \
  ? ((int16_t)(left)) \
  : (((int16_t)(left)) << ((unsigned int)(right)))))

#define c99_strict_safe_rshift_func_int16_t_s_s(left,_left,right,_right) \
    ((int16_t)( left = (_left), right = (_right) , \
        ((((int16_t)(left)) < ((int16_t)0)) \
             || (((int)(right)) < ((int16_t)0)) \
             || (((int)(right)) >= sizeof(int16_t)*CHAR_BIT)) \
            ? ((int16_t)(left)) \
            : (((int16_t)(left)) >> ((int)(right)))))

#define c99_strict_safe_rshift_func_int16_t_s_u(left,_left,right,_right) \
  ((int16_t)( left = (_left), right = (_right) , \
   ((((int16_t)(left)) < ((int16_t)0)) \
             || (((unsigned int)(right)) >= sizeof(int16_t)*CHAR_BIT)) \
            ? ((int16_t)(left)) \
            : (((int16_t)(left)) >> ((unsigned int)(right)))))

#define safe_rshift_func_int16_t_s_s(left,_left,right,_right) \
    ((int16_t)( left = (_left), right = (_right) , \
        ( \
              (((int)(right)) < ((int16_t)0)) \
             || (((int)(right)) >= sizeof(int16_t)*CHAR_BIT)) \
            ? ((int16_t)(left)) \
            : (((int16_t)(left)) >> ((int)(right)))))

#define safe_rshift_func_int16_t_s_u(left,_left,right,_right) \
  ((int16_t)( left = (_left), right = (_right) , \
   ( \
    (((unsigned int)(right)) >= sizeof(int16_t)*CHAR_BIT)) \
    ? ((int16_t)(left)) \
    : (((int16_t)(left)) >> ((unsigned int)(right)))))



#define safe_unary_minus_func_int32_t_s(si,_si) \
  ((int32_t)( si = (_si), \
   (((int32_t)(si))==(INT32_MIN))? \
    ((int32_t)(si)): \
    (-((int32_t)(si))) \
  ))

#define safe_add_func_int32_t_s_s(si1,_si1,si2,_si2) \
        ((int32_t)( si1 = (_si1), si2 = (_si2) , \
                 (((((int32_t)(si1))>((int32_t)0)) && (((int32_t)(si2))>((int32_t)0)) && (((int32_t)(si1)) > ((INT32_MAX)-((int32_t)(si2))))) \
          || ((((int32_t)(si1))<((int32_t)0)) && (((int32_t)(si2))<((int32_t)0)) && (((int32_t)(si1)) < ((INT32_MIN)-((int32_t)(si2)))))) ? \
         ((int32_t)(si1)) :                     \
         (((int32_t)(si1)) + ((int32_t)(si2)))              \
        )) 

#define safe_sub_func_int32_t_s_s(si1,_si1,si2,_si2) \
        ((int32_t)( si1 = (_si1), si2 = (_si2) , \
                (((((int32_t)(si1))^((int32_t)(si2))) \
        & (((((int32_t)(si1)) ^ ((((int32_t)(si1))^((int32_t)(si2))) \
        & (((int32_t)1) << (sizeof(int32_t)*CHAR_BIT-1))))-((int32_t)(si2)))^((int32_t)(si2)))) < ((int32_t)0)) \
        ? ((int32_t)(si1)) \
        : (((int32_t)(si1)) - ((int32_t)(si2))) \
        ))

#define safe_mul_func_int32_t_s_s(si1,_si1,si2,_si2) \
  ((int32_t)( si1 = (_si1), si2 = (_si2) , \
  (((((int32_t)(si1)) > ((int32_t)0)) && (((int32_t)(si2)) > ((int32_t)0)) && (((int32_t)(si1)) > ((INT32_MAX) / ((int32_t)(si2))))) || \
  ((((int32_t)(si1)) > ((int32_t)0)) && (((int32_t)(si2)) <= ((int32_t)0)) && (((int32_t)(si2)) < ((INT32_MIN) / ((int32_t)(si1))))) || \
  ((((int32_t)(si1)) <= ((int32_t)0)) && (((int32_t)(si2)) > ((int32_t)0)) && (((int32_t)(si1)) < ((INT32_MIN) / ((int32_t)(si2))))) || \
  ((((int32_t)(si1)) <= ((int32_t)0)) && (((int32_t)(si2)) <= ((int32_t)0)) && (((int32_t)(si1)) != ((int32_t)0)) && (((int32_t)(si2)) < ((INT32_MAX) / ((int32_t)(si1)))))) \
  ? ((int32_t)(si1)) \
  : ((int32_t)(si1)) * ((int32_t)(si2))))

#define safe_mod_func_int32_t_s_s(si1,_si1,si2,_si2) \
  ((int32_t)( si1 = (_si1), si2 = (_si2) , \
  ((((int32_t)(si2)) == ((int32_t)0)) || ((((int32_t)(si1)) == (INT32_MIN)) && (((int32_t)(si2)) == ((int32_t)-1)))) \
  ? ((int32_t)(si1)) \
  : (((int32_t)(si1)) % ((int32_t)(si2)))))

#define safe_div_func_int32_t_s_s(si1,_si1,si2,_si2) \
  ((int32_t)( si1 = (_si1), si2 = (_si2) , \
  ((((int32_t)(si2)) == ((int32_t)0)) || ((((int32_t)(si1)) == (INT32_MIN)) && (((int32_t)(si2)) == ((int32_t)-1)))) \
  ? ((int32_t)(si1)) \
  : (((int32_t)(si1)) / ((int32_t)(si2)))))

#define c99_strict_safe_lshift_func_int32_t_s_s(left,_left,right,_right) \
  ((int32_t)( left = (_left), right = (_right) , \
   ( \
   (((int32_t)(left)) < ((int32_t)0)) \
  || (((int)(right)) < ((int32_t)0)) \
  || (((int)(right)) >= sizeof(int32_t)*CHAR_BIT) \
  || (((int32_t)(left)) > ((INT32_MAX) >> ((int)(right))))) \
  ? ((int32_t)(left)) \
  : (((int32_t)(left)) << ((int)(right)))))

#define safe_lshift_func_int32_t_s_s(left,_left,right,_right) \
  ((int32_t)( left = (_left), right = (_right) , \
   ( \
    (((int)(right)) < ((int32_t)0)) \
  || (((int)(right)) >= sizeof(int32_t)*CHAR_BIT) \
   ) \
  ? ((int32_t)(left)) \
  : (((int32_t)(left)) << ((int)(right)))))

#define c99_strict_safe_lshift_func_int32_t_s_u(left,_left,right,_right) \
  ((int32_t)( left = (_left), right = (_right) , \
   ((((int32_t)(left)) < ((int32_t)0)) \
  || (((unsigned int)(right)) >= sizeof(int32_t)*CHAR_BIT) \
  || (((int32_t)(left)) > ((INT32_MAX) >> ((unsigned int)(right))))) \
  ? ((int32_t)(left)) \
  : (((int32_t)(left)) << ((unsigned int)(right)))))

#define safe_lshift_func_int32_t_s_u(left,_left,right,_right) \
  ((int32_t)( left = (_left), right = (_right) , \
   ( \
   (((unsigned int)(right)) >= sizeof(int32_t)*CHAR_BIT) \
   ) \
  ? ((int32_t)(left)) \
  : (((int32_t)(left)) << ((unsigned int)(right)))))

#define c99_strict_safe_rshift_func_int32_t_s_s(left,_left,right,_right) \
    ((int32_t)( left = (_left), right = (_right) , \
        ((((int32_t)(left)) < ((int32_t)0)) \
             || (((int)(right)) < ((int32_t)0)) \
             || (((int)(right)) >= sizeof(int32_t)*CHAR_BIT)) \
            ? ((int32_t)(left)) \
            : (((int32_t)(left)) >> ((int)(right)))))

#define c99_strict_safe_rshift_func_int32_t_s_u(left,_left,right,_right) \
  ((int32_t)( left = (_left), right = (_right) , \
   ((((int32_t)(left)) < ((int32_t)0)) \
             || (((unsigned int)(right)) >= sizeof(int32_t)*CHAR_BIT)) \
            ? ((int32_t)(left)) \
            : (((int32_t)(left)) >> ((unsigned int)(right)))))

#define safe_rshift_func_int32_t_s_s(left,_left,right,_right) \
    ((int32_t)( left = (_left), right = (_right) , \
        ( \
              (((int)(right)) < ((int32_t)0)) \
             || (((int)(right)) >= sizeof(int32_t)*CHAR_BIT)) \
            ? ((int32_t)(left)) \
            : (((int32_t)(left)) >> ((int)(right)))))

#define safe_rshift_func_int32_t_s_u(left,_left,right,_right) \
  ((int32_t)( left = (_left), right = (_right) , \
   ( \
    (((unsigned int)(right)) >= sizeof(int32_t)*CHAR_BIT)) \
    ? ((int32_t)(left)) \
    : (((int32_t)(left)) >> ((unsigned int)(right)))))



#define safe_unary_minus_func_int64_t_s(si,_si) \
  ((int64_t)( si = (_si), \
   (((int64_t)(si))==(INT64_MIN))? \
    ((int64_t)(si)): \
    (-((int64_t)(si))) \
  ))

#define safe_add_func_int64_t_s_s(si1,_si1,si2,_si2) \
        ((int64_t)( si1 = (_si1), si2 = (_si2) , \
                 (((((int64_t)(si1))>((int64_t)0)) && (((int64_t)(si2))>((int64_t)0)) && (((int64_t)(si1)) > ((INT64_MAX)-((int64_t)(si2))))) \
          || ((((int64_t)(si1))<((int64_t)0)) && (((int64_t)(si2))<((int64_t)0)) && (((int64_t)(si1)) < ((INT64_MIN)-((int64_t)(si2)))))) ? \
         ((int64_t)(si1)) :                     \
         (((int64_t)(si1)) + ((int64_t)(si2)))              \
        )) 

#define safe_sub_func_int64_t_s_s(si1,_si1,si2,_si2) \
        ((int64_t)( si1 = (_si1), si2 = (_si2) , \
                (((((int64_t)(si1))^((int64_t)(si2))) \
        & (((((int64_t)(si1)) ^ ((((int64_t)(si1))^((int64_t)(si2))) \
        & (((int64_t)1) << (sizeof(int64_t)*CHAR_BIT-1))))-((int64_t)(si2)))^((int64_t)(si2)))) < ((int64_t)0)) \
        ? ((int64_t)(si1)) \
        : (((int64_t)(si1)) - ((int64_t)(si2))) \
        ))

#define safe_mul_func_int64_t_s_s(si1,_si1,si2,_si2) \
  ((int64_t)( si1 = (_si1), si2 = (_si2) , \
  (((((int64_t)(si1)) > ((int64_t)0)) && (((int64_t)(si2)) > ((int64_t)0)) && (((int64_t)(si1)) > ((INT64_MAX) / ((int64_t)(si2))))) || \
  ((((int64_t)(si1)) > ((int64_t)0)) && (((int64_t)(si2)) <= ((int64_t)0)) && (((int64_t)(si2)) < ((INT64_MIN) / ((int64_t)(si1))))) || \
  ((((int64_t)(si1)) <= ((int64_t)0)) && (((int64_t)(si2)) > ((int64_t)0)) && (((int64_t)(si1)) < ((INT64_MIN) / ((int64_t)(si2))))) || \
  ((((int64_t)(si1)) <= ((int64_t)0)) && (((int64_t)(si2)) <= ((int64_t)0)) && (((int64_t)(si1)) != ((int64_t)0)) && (((int64_t)(si2)) < ((INT64_MAX) / ((int64_t)(si1)))))) \
  ? ((int64_t)(si1)) \
  : ((int64_t)(si1)) * ((int64_t)(si2))))

#define safe_mod_func_int64_t_s_s(si1,_si1,si2,_si2) \
  ((int64_t)( si1 = (_si1), si2 = (_si2) , \
  ((((int64_t)(si2)) == ((int64_t)0)) || ((((int64_t)(si1)) == (INT64_MIN)) && (((int64_t)(si2)) == ((int64_t)-1)))) \
  ? ((int64_t)(si1)) \
  : (((int64_t)(si1)) % ((int64_t)(si2)))))

#define safe_div_func_int64_t_s_s(si1,_si1,si2,_si2) \
  ((int64_t)( si1 = (_si1), si2 = (_si2) , \
  ((((int64_t)(si2)) == ((int64_t)0)) || ((((int64_t)(si1)) == (INT64_MIN)) && (((int64_t)(si2)) == ((int64_t)-1)))) \
  ? ((int64_t)(si1)) \
  : (((int64_t)(si1)) / ((int64_t)(si2)))))

#define c99_strict_safe_lshift_func_int64_t_s_s(left,_left,right,_right) \
  ((int64_t)( left = (_left), right = (_right) , \
   ( \
   (((int64_t)(left)) < ((int64_t)0)) \
  || (((int)(right)) < ((int64_t)0)) \
  || (((int)(right)) >= sizeof(int64_t)*CHAR_BIT) \
  || (((int64_t)(left)) > ((INT64_MAX) >> ((int)(right))))) \
  ? ((int64_t)(left)) \
  : (((int64_t)(left)) << ((int)(right)))))

#define safe_lshift_func_int64_t_s_s(left,_left,right,_right) \
  ((int64_t)( left = (_left), right = (_right) , \
   ( \
    (((int)(right)) < ((int64_t)0)) \
  || (((int)(right)) >= sizeof(int64_t)*CHAR_BIT) \
   ) \
  ? ((int64_t)(left)) \
  : (((int64_t)(left)) << ((int)(right)))))

#define c99_strict_safe_lshift_func_int64_t_s_u(left,_left,right,_right) \
  ((int64_t)( left = (_left), right = (_right) , \
   ((((int64_t)(left)) < ((int64_t)0)) \
  || (((unsigned int)(right)) >= sizeof(int64_t)*CHAR_BIT) \
  || (((int64_t)(left)) > ((INT64_MAX) >> ((unsigned int)(right))))) \
  ? ((int64_t)(left)) \
  : (((int64_t)(left)) << ((unsigned int)(right)))))

#define safe_lshift_func_int64_t_s_u(left,_left,right,_right) \
  ((int64_t)( left = (_left), right = (_right) , \
   ( \
   (((unsigned int)(right)) >= sizeof(int64_t)*CHAR_BIT) \
   ) \
  ? ((int64_t)(left)) \
  : (((int64_t)(left)) << ((unsigned int)(right)))))

#define c99_strict_safe_rshift_func_int64_t_s_s(left,_left,right,_right) \
    ((int64_t)( left = (_left), right = (_right) , \
        ((((int64_t)(left)) < ((int64_t)0)) \
             || (((int)(right)) < ((int64_t)0)) \
             || (((int)(right)) >= sizeof(int64_t)*CHAR_BIT)) \
            ? ((int64_t)(left)) \
            : (((int64_t)(left)) >> ((int)(right)))))

#define c99_strict_safe_rshift_func_int64_t_s_u(left,_left,right,_right) \
  ((int64_t)( left = (_left), right = (_right) , \
   ((((int64_t)(left)) < ((int64_t)0)) \
             || (((unsigned int)(right)) >= sizeof(int64_t)*CHAR_BIT)) \
            ? ((int64_t)(left)) \
            : (((int64_t)(left)) >> ((unsigned int)(right)))))

#define safe_rshift_func_int64_t_s_s(left,_left,right,_right) \
    ((int64_t)( left = (_left), right = (_right) , \
        ( \
              (((int)(right)) < ((int64_t)0)) \
             || (((int)(right)) >= sizeof(int64_t)*CHAR_BIT)) \
            ? ((int64_t)(left)) \
            : (((int64_t)(left)) >> ((int)(right)))))

#define safe_rshift_func_int64_t_s_u(left,_left,right,_right) \
  ((int64_t)( left = (_left), right = (_right) , \
   ( \
    (((unsigned int)(right)) >= sizeof(int64_t)*CHAR_BIT)) \
    ? ((int64_t)(left)) \
    : (((int64_t)(left)) >> ((unsigned int)(right)))))








#define safe_unary_minus_func_uint8_t_u(ui,_ui) \
  ((uint8_t)( ui = (_ui), -((uint8_t)(ui))))

#define safe_add_func_uint8_t_u_u(ui1,_ui1,ui2,_ui2) \
  ((uint8_t)( ui1 = (_ui1), ui2 = (_ui2) , \
  ((uint8_t)(ui1)) + ((uint8_t)(ui2))))

#define safe_sub_func_uint8_t_u_u(ui1,_ui1,ui2,_ui2) \
  ((uint8_t)( ui1 = (_ui1), ui2 = (_ui2) , ((uint8_t)(ui1)) - ((uint8_t)(ui2))))

#define safe_mul_func_uint8_t_u_u(ui1,_ui1,ui2,_ui2) \
  ((uint8_t)(( ui1 = (_ui1), ui2 = (_ui2) , (uint8_t)(((unsigned int)(ui1)) * ((unsigned int)(ui2))))))

#define safe_mod_func_uint8_t_u_u(ui1,_ui1,ui2,_ui2) \
    ((uint8_t)( ui1 = (_ui1), ui2 = (_ui2) , \
         (((uint8_t)(ui2)) == ((uint8_t)0)) \
            ? ((uint8_t)(ui1)) \
            : (((uint8_t)(ui1)) % ((uint8_t)(ui2)))))

#define safe_div_func_uint8_t_u_u(ui1,_ui1,ui2,_ui2) \
            ((uint8_t)( ui1 = (_ui1), ui2 = (_ui2) , \
                 (((uint8_t)(ui2)) == ((uint8_t)0)) \
            ? ((uint8_t)(ui1)) \
            : (((uint8_t)(ui1)) / ((uint8_t)(ui2)))))

#define c99_strict_safe_lshift_func_uint8_t_u_s(left,_left,right,_right) \
    ((uint8_t)( left = (_left), right = (_right) , \
          ((((int)(right)) < ((uint8_t)0)) \
             || (((int)(right)) >= sizeof(uint8_t)*CHAR_BIT) \
             || (((uint8_t)(left)) > ((UINT8_MAX) >> ((int)(right))))) \
            ? ((uint8_t)(left)) \
            : (((uint8_t)(left)) << ((int)(right)))))

#define c99_strict_safe_lshift_func_uint8_t_u_u(left,_left,right,_right) \
     ((uint8_t)( left = (_left), right = (_right) , \
           ((((unsigned int)(right)) >= sizeof(uint8_t)*CHAR_BIT) \
             || (((uint8_t)(left)) > ((UINT8_MAX) >> ((unsigned int)(right))))) \
            ? ((uint8_t)(left)) \
            : (((uint8_t)(left)) << ((unsigned int)(right)))))

#define c99_strict_safe_rshift_func_uint8_t_u_s(left,_left,right,_right) \
    ((uint8_t)( left = (_left), right = (_right) , \
          ((((int)(right)) < ((uint8_t)0)) \
             || (((int)(right)) >= sizeof(uint8_t)*CHAR_BIT)) \
            ? ((uint8_t)(left)) \
            : (((uint8_t)(left)) >> ((int)(right)))))

#define c99_strict_safe_rshift_func_uint8_t_u_u(left,_left,right,_right) \
    ((uint8_t)( left = (_left), right = (_right) , \
                 (((unsigned int)(right)) >= sizeof(uint8_t)*CHAR_BIT) \
             ? ((uint8_t)(left)) \
             : (((uint8_t)(left)) >> ((unsigned int)(right)))))

#define safe_lshift_func_uint8_t_u_s(left,_left,right,_right) \
    ((uint8_t)( left = (_left), right = (_right) , \
          ((((int)(right)) < ((uint8_t)0)) \
             || (((int)(right)) >= sizeof(uint8_t)*CHAR_BIT) \
            ) \
            ? ((uint8_t)(left)) \
            : (((uint8_t)(left)) << ((int)(right)))))

#define safe_lshift_func_uint8_t_u_u(left,_left,right,_right) \
     ((uint8_t)( left = (_left), right = (_right) , \
           ((((unsigned int)(right)) >= sizeof(uint8_t)*CHAR_BIT)) \
            ? ((uint8_t)(left)) \
            : (((uint8_t)(left)) << ((unsigned int)(right)))))

#define safe_rshift_func_uint8_t_u_s(left,_left,right,_right) \
    ((uint8_t)( left = (_left), right = (_right) , \
          ((((int)(right)) < ((uint8_t)0)) \
             || (((int)(right)) >= sizeof(uint8_t)*CHAR_BIT)) \
            ? ((uint8_t)(left)) \
            : (((uint8_t)(left)) >> ((int)(right)))))

#define safe_rshift_func_uint8_t_u_u(left,_left,right,_right) \
    ((uint8_t)( left = (_left), right = (_right) , \
                 (((unsigned int)(right)) >= sizeof(uint8_t)*CHAR_BIT) \
             ? ((uint8_t)(left)) \
             : (((uint8_t)(left)) >> ((unsigned int)(right)))))




#define safe_unary_minus_func_uint16_t_u(ui,_ui) \
  ((uint16_t)( ui = (_ui), -((uint16_t)(ui))))

#define safe_add_func_uint16_t_u_u(ui1,_ui1,ui2,_ui2) \
  ((uint16_t)( ui1 = (_ui1), ui2 = (_ui2) , \
  ((uint16_t)(ui1)) + ((uint16_t)(ui2))))

#define safe_sub_func_uint16_t_u_u(ui1,_ui1,ui2,_ui2) \
  ((uint16_t)( ui1 = (_ui1), ui2 = (_ui2) , ((uint16_t)(ui1)) - ((uint16_t)(ui2))))

#define safe_mul_func_uint16_t_u_u(ui1,_ui1,ui2,_ui2) \
  ((uint16_t)(( ui1 = (_ui1), ui2 = (_ui2) , (uint16_t)(((unsigned int)(ui1)) * ((unsigned int)(ui2))))))

#define safe_mod_func_uint16_t_u_u(ui1,_ui1,ui2,_ui2) \
    ((uint16_t)( ui1 = (_ui1), ui2 = (_ui2) , \
         (((uint16_t)(ui2)) == ((uint16_t)0)) \
            ? ((uint16_t)(ui1)) \
            : (((uint16_t)(ui1)) % ((uint16_t)(ui2)))))

#define safe_div_func_uint16_t_u_u(ui1,_ui1,ui2,_ui2) \
            ((uint16_t)( ui1 = (_ui1), ui2 = (_ui2) , \
                 (((uint16_t)(ui2)) == ((uint16_t)0)) \
            ? ((uint16_t)(ui1)) \
            : (((uint16_t)(ui1)) / ((uint16_t)(ui2)))))

#define c99_strict_safe_lshift_func_uint16_t_u_s(left,_left,right,_right) \
    ((uint16_t)( left = (_left), right = (_right) , \
          ((((int)(right)) < ((uint16_t)0)) \
             || (((int)(right)) >= sizeof(uint16_t)*CHAR_BIT) \
             || (((uint16_t)(left)) > ((UINT16_MAX) >> ((int)(right))))) \
            ? ((uint16_t)(left)) \
            : (((uint16_t)(left)) << ((int)(right)))))

#define c99_strict_safe_lshift_func_uint16_t_u_u(left,_left,right,_right) \
     ((uint16_t)( left = (_left), right = (_right) , \
           ((((unsigned int)(right)) >= sizeof(uint16_t)*CHAR_BIT) \
             || (((uint16_t)(left)) > ((UINT16_MAX) >> ((unsigned int)(right))))) \
            ? ((uint16_t)(left)) \
            : (((uint16_t)(left)) << ((unsigned int)(right)))))

#define c99_strict_safe_rshift_func_uint16_t_u_s(left,_left,right,_right) \
    ((uint16_t)( left = (_left), right = (_right) , \
          ((((int)(right)) < ((uint16_t)0)) \
             || (((int)(right)) >= sizeof(uint16_t)*CHAR_BIT)) \
            ? ((uint16_t)(left)) \
            : (((uint16_t)(left)) >> ((int)(right)))))

#define c99_strict_safe_rshift_func_uint16_t_u_u(left,_left,right,_right) \
    ((uint16_t)( left = (_left), right = (_right) , \
                 (((unsigned int)(right)) >= sizeof(uint16_t)*CHAR_BIT) \
             ? ((uint16_t)(left)) \
             : (((uint16_t)(left)) >> ((unsigned int)(right)))))

#define safe_lshift_func_uint16_t_u_s(left,_left,right,_right) \
    ((uint16_t)( left = (_left), right = (_right) , \
          ((((int)(right)) < ((uint16_t)0)) \
             || (((int)(right)) >= sizeof(uint16_t)*CHAR_BIT) \
            ) \
            ? ((uint16_t)(left)) \
            : (((uint16_t)(left)) << ((int)(right)))))

#define safe_lshift_func_uint16_t_u_u(left,_left,right,_right) \
     ((uint16_t)( left = (_left), right = (_right) , \
           ((((unsigned int)(right)) >= sizeof(uint16_t)*CHAR_BIT)) \
            ? ((uint16_t)(left)) \
            : (((uint16_t)(left)) << ((unsigned int)(right)))))

#define safe_rshift_func_uint16_t_u_s(left,_left,right,_right) \
    ((uint16_t)( left = (_left), right = (_right) , \
          ((((int)(right)) < ((uint16_t)0)) \
             || (((int)(right)) >= sizeof(uint16_t)*CHAR_BIT)) \
            ? ((uint16_t)(left)) \
            : (((uint16_t)(left)) >> ((int)(right)))))

#define safe_rshift_func_uint16_t_u_u(left,_left,right,_right) \
    ((uint16_t)( left = (_left), right = (_right) , \
                 (((unsigned int)(right)) >= sizeof(uint16_t)*CHAR_BIT) \
             ? ((uint16_t)(left)) \
             : (((uint16_t)(left)) >> ((unsigned int)(right)))))




#define safe_unary_minus_func_uint32_t_u(ui,_ui) \
  ((uint32_t)( ui = (_ui), -((uint32_t)(ui))))

#define safe_add_func_uint32_t_u_u(ui1,_ui1,ui2,_ui2) \
  ((uint32_t)( ui1 = (_ui1), ui2 = (_ui2) , \
  ((uint32_t)(ui1)) + ((uint32_t)(ui2))))

#define safe_sub_func_uint32_t_u_u(ui1,_ui1,ui2,_ui2) \
  ((uint32_t)( ui1 = (_ui1), ui2 = (_ui2) , ((uint32_t)(ui1)) - ((uint32_t)(ui2))))

#define safe_mul_func_uint32_t_u_u(ui1,_ui1,ui2,_ui2) \
  ((uint32_t)(( ui1 = (_ui1), ui2 = (_ui2) , (uint32_t)(((unsigned int)(ui1)) * ((unsigned int)(ui2))))))

#define safe_mod_func_uint32_t_u_u(ui1,_ui1,ui2,_ui2) \
    ((uint32_t)( ui1 = (_ui1), ui2 = (_ui2) , \
         (((uint32_t)(ui2)) == ((uint32_t)0)) \
            ? ((uint32_t)(ui1)) \
            : (((uint32_t)(ui1)) % ((uint32_t)(ui2)))))

#define safe_div_func_uint32_t_u_u(ui1,_ui1,ui2,_ui2) \
            ((uint32_t)( ui1 = (_ui1), ui2 = (_ui2) , \
                 (((uint32_t)(ui2)) == ((uint32_t)0)) \
            ? ((uint32_t)(ui1)) \
            : (((uint32_t)(ui1)) / ((uint32_t)(ui2)))))

#define c99_strict_safe_lshift_func_uint32_t_u_s(left,_left,right,_right) \
    ((uint32_t)( left = (_left), right = (_right) , \
          ((((int)(right)) < ((uint32_t)0)) \
             || (((int)(right)) >= sizeof(uint32_t)*CHAR_BIT) \
             || (((uint32_t)(left)) > ((UINT32_MAX) >> ((int)(right))))) \
            ? ((uint32_t)(left)) \
            : (((uint32_t)(left)) << ((int)(right)))))

#define c99_strict_safe_lshift_func_uint32_t_u_u(left,_left,right,_right) \
     ((uint32_t)( left = (_left), right = (_right) , \
           ((((unsigned int)(right)) >= sizeof(uint32_t)*CHAR_BIT) \
             || (((uint32_t)(left)) > ((UINT32_MAX) >> ((unsigned int)(right))))) \
            ? ((uint32_t)(left)) \
            : (((uint32_t)(left)) << ((unsigned int)(right)))))

#define c99_strict_safe_rshift_func_uint32_t_u_s(left,_left,right,_right) \
    ((uint32_t)( left = (_left), right = (_right) , \
          ((((int)(right)) < ((uint32_t)0)) \
             || (((int)(right)) >= sizeof(uint32_t)*CHAR_BIT)) \
            ? ((uint32_t)(left)) \
            : (((uint32_t)(left)) >> ((int)(right)))))

#define c99_strict_safe_rshift_func_uint32_t_u_u(left,_left,right,_right) \
    ((uint32_t)( left = (_left), right = (_right) , \
                 (((unsigned int)(right)) >= sizeof(uint32_t)*CHAR_BIT) \
             ? ((uint32_t)(left)) \
             : (((uint32_t)(left)) >> ((unsigned int)(right)))))

#define safe_lshift_func_uint32_t_u_s(left,_left,right,_right) \
    ((uint32_t)( left = (_left), right = (_right) , \
          ((((int)(right)) < ((uint32_t)0)) \
             || (((int)(right)) >= sizeof(uint32_t)*CHAR_BIT) \
            ) \
            ? ((uint32_t)(left)) \
            : (((uint32_t)(left)) << ((int)(right)))))

#define safe_lshift_func_uint32_t_u_u(left,_left,right,_right) \
     ((uint32_t)( left = (_left), right = (_right) , \
           ((((unsigned int)(right)) >= sizeof(uint32_t)*CHAR_BIT)) \
            ? ((uint32_t)(left)) \
            : (((uint32_t)(left)) << ((unsigned int)(right)))))

#define safe_rshift_func_uint32_t_u_s(left,_left,right,_right) \
    ((uint32_t)( left = (_left), right = (_right) , \
          ((((int)(right)) < ((uint32_t)0)) \
             || (((int)(right)) >= sizeof(uint32_t)*CHAR_BIT)) \
            ? ((uint32_t)(left)) \
            : (((uint32_t)(left)) >> ((int)(right)))))

#define safe_rshift_func_uint32_t_u_u(left,_left,right,_right) \
    ((uint32_t)( left = (_left), right = (_right) , \
                 (((unsigned int)(right)) >= sizeof(uint32_t)*CHAR_BIT) \
             ? ((uint32_t)(left)) \
             : (((uint32_t)(left)) >> ((unsigned int)(right)))))




#define safe_unary_minus_func_uint64_t_u(ui,_ui) \
  ((uint64_t)( ui = (_ui), -((uint64_t)(ui))))

#define safe_add_func_uint64_t_u_u(ui1,_ui1,ui2,_ui2) \
  ((uint64_t)( ui1 = (_ui1), ui2 = (_ui2) , \
  ((uint64_t)(ui1)) + ((uint64_t)(ui2))))

#define safe_sub_func_uint64_t_u_u(ui1,_ui1,ui2,_ui2) \
  ((uint64_t)( ui1 = (_ui1), ui2 = (_ui2) , ((uint64_t)(ui1)) - ((uint64_t)(ui2))))

#define safe_mul_func_uint64_t_u_u(ui1,_ui1,ui2,_ui2) \
  ((uint64_t)(( ui1 = (_ui1), ui2 = (_ui2) , (uint64_t)(((unsigned long long)(ui1)) * ((unsigned long long)(ui2))))))

#define safe_mod_func_uint64_t_u_u(ui1,_ui1,ui2,_ui2) \
    ((uint64_t)( ui1 = (_ui1), ui2 = (_ui2) , \
         (((uint64_t)(ui2)) == ((uint64_t)0)) \
            ? ((uint64_t)(ui1)) \
            : (((uint64_t)(ui1)) % ((uint64_t)(ui2)))))

#define safe_div_func_uint64_t_u_u(ui1,_ui1,ui2,_ui2) \
            ((uint64_t)( ui1 = (_ui1), ui2 = (_ui2) , \
                 (((uint64_t)(ui2)) == ((uint64_t)0)) \
            ? ((uint64_t)(ui1)) \
            : (((uint64_t)(ui1)) / ((uint64_t)(ui2)))))

#define c99_strict_safe_lshift_func_uint64_t_u_s(left,_left,right,_right) \
    ((uint64_t)( left = (_left), right = (_right) , \
          ((((int)(right)) < ((uint64_t)0)) \
             || (((int)(right)) >= sizeof(uint64_t)*CHAR_BIT) \
             || (((uint64_t)(left)) > ((UINT64_MAX) >> ((int)(right))))) \
            ? ((uint64_t)(left)) \
            : (((uint64_t)(left)) << ((int)(right)))))

#define c99_strict_safe_lshift_func_uint64_t_u_u(left,_left,right,_right) \
     ((uint64_t)( left = (_left), right = (_right) , \
           ((((unsigned int)(right)) >= sizeof(uint64_t)*CHAR_BIT) \
             || (((uint64_t)(left)) > ((UINT64_MAX) >> ((unsigned int)(right))))) \
            ? ((uint64_t)(left)) \
            : (((uint64_t)(left)) << ((unsigned int)(right)))))

#define c99_strict_safe_rshift_func_uint64_t_u_s(left,_left,right,_right) \
    ((uint64_t)( left = (_left), right = (_right) , \
          ((((int)(right)) < ((uint64_t)0)) \
             || (((int)(right)) >= sizeof(uint64_t)*CHAR_BIT)) \
            ? ((uint64_t)(left)) \
            : (((uint64_t)(left)) >> ((int)(right)))))

#define c99_strict_safe_rshift_func_uint64_t_u_u(left,_left,right,_right) \
    ((uint64_t)( left = (_left), right = (_right) , \
                 (((unsigned int)(right)) >= sizeof(uint64_t)*CHAR_BIT) \
             ? ((uint64_t)(left)) \
             : (((uint64_t)(left)) >> ((unsigned int)(right)))))

#define safe_lshift_func_uint64_t_u_s(left,_left,right,_right) \
    ((uint64_t)( left = (_left), right = (_right) , \
          ((((int)(right)) < ((uint64_t)0)) \
             || (((int)(right)) >= sizeof(uint64_t)*CHAR_BIT) \
            ) \
            ? ((uint64_t)(left)) \
            : (((uint64_t)(left)) << ((int)(right)))))

#define safe_lshift_func_uint64_t_u_u(left,_left,right,_right) \
     ((uint64_t)( left = (_left), right = (_right) , \
           ((((unsigned int)(right)) >= sizeof(uint64_t)*CHAR_BIT)) \
            ? ((uint64_t)(left)) \
            : (((uint64_t)(left)) << ((unsigned int)(right)))))

#define safe_rshift_func_uint64_t_u_s(left,_left,right,_right) \
    ((uint64_t)( left = (_left), right = (_right) , \
          ((((int)(right)) < ((uint64_t)0)) \
             || (((int)(right)) >= sizeof(uint64_t)*CHAR_BIT)) \
            ? ((uint64_t)(left)) \
            : (((uint64_t)(left)) >> ((int)(right)))))

#define safe_rshift_func_uint64_t_u_u(left,_left,right,_right) \
    ((uint64_t)( left = (_left), right = (_right) , \
                 (((unsigned int)(right)) >= sizeof(uint64_t)*CHAR_BIT) \
             ? ((uint64_t)(left)) \
             : (((uint64_t)(left)) >> ((unsigned int)(right)))))

*/