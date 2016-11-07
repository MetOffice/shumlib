/* *********************************COPYRIGHT**********************************/
/* (C) Crown copyright Met Office. All rights reserved.                       */
/* For further details please refer to the file LICENCE.txt                   */
/* which you should have received as part of this distribution.               */
/* *********************************COPYRIGHT**********************************/
/*                                                                            */
/* This file is part of the UM Shared Library project.                        */
/*                                                                            */
/* The UM Shared Library is free software: you can redistribute it            */
/* and/or modify it under the terms of the Modified BSD License, as           */
/* published by the Open Source Initiative.                                   */
/*                                                                            */
/* The UM Shared Library is distributed in the hope that it will be           */
/* useful, but WITHOUT ANY WARRANTY; without even the implied warranty        */
/* of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the           */
/* Modified BSD License for more details.                                     */
/*                                                                            */
/* You should have received a copy of the Modified BSD License                */
/* along with the UM Shared Library.                                          */
/* If not, see <http://opensource.org/licenses/BSD-3-Clause>.                 */
/******************************************************************************/

#include <stdio.h>
#include "c_shum_read_wgdos_header.h"
#include "c_shum_data_conv.h"

/* The important header quantities are stored in specific words of the
 * field data; these macros give the offest in bytes to these words
 */
#define first_word_offset 0  
#define second_word_offset 8 

/* This function returns the accuracy, number of rows and number of    
 * columns in a WGDOS field header, given the bytes (must already be   
 * byte-swapped if required)                                           
 */
int64_t c_shum_read_wgdos_header(char    *bytes_in,
                                 int64_t *accuracy,
                                 int64_t *cols,
                                 int64_t *rows,
                                 char    *message)
{
  // Fixed parameters for IBM2IEEE calls
  c_shum_datatypes int_type = C_SHUM_INTEGER;
  int64_t num = 1;
  int64_t size_out = 64;
  int64_t stride = 1;

  // Variables for IBM2IEEE calls
  int64_t offset_in;
  int64_t size_in;
  int64_t status;

  // Extract accuracy from field-header with IBM2IEEE
  offset_in = 32;
  size_in = 32;
  status = c_shum_ibm2ieee(&int_type, &num, &bytes_in[first_word_offset], 
                           &offset_in, accuracy, &stride, &size_out, &size_in, 
                           message);
  if (status != 0) return status;

  // Extract number of columns from field-header with IBM2IEEE
  offset_in = 0;
  size_in = 16;
  status = c_shum_ibm2ieee(&int_type, &num, &bytes_in[second_word_offset], 
                           &offset_in, cols, &stride, &size_out, &size_in, 
                           message);
  if (status != 0) return status;

  // Extract number of rows from field-header with IBM2IEEE
  offset_in = 16;
  size_in = 16;
  status = c_shum_ibm2ieee(&int_type, &num, &bytes_in[second_word_offset], 
                           &offset_in, rows, &stride, &size_out, &size_in, 
                           message);
  return status;
}

  
