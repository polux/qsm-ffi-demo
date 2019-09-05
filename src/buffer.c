/*
 * Copyright 2019 Google LLC
 *
 * Use of this source code is governed by a BSD-style
 * license that can be found in the LICENSE file or at
 * https://developers.google.com/open-source/licenses/bsd
 */

#include <stdlib.h>
#include <stdio.h>
#include "buffer.h"

Buffer* new_buffer(int size) {
  Buffer* buffer = (Buffer*) malloc(sizeof(buffer));
  buffer->top = 0;
  buffer->bot = 0;
  buffer->size = size + 1;
  buffer->arr = (int*) malloc(buffer->size * sizeof(int));
  return buffer;
}

void put_buffer(int i, Buffer* buffer) {
  buffer->arr[buffer->top] = i;
  buffer->top = (buffer->top + 1) % buffer->size;
}

int get_buffer(Buffer* buffer) {
  int res = buffer->arr[buffer->bot];
  buffer->bot = (buffer->bot + 1) % buffer->size;
  return res;
}

int len_buffer(Buffer* buffer) {
  return (buffer->top - buffer->bot + buffer->size) % buffer->size;
}

bool same_elements(Buffer* buffer1, Buffer* buffer2) {
  if (len_buffer(buffer1) != len_buffer(buffer2))
    return false;
  int i1 = buffer1->bot;
  int i2 = buffer2->bot;
  while(i1 != buffer1->top) {
    if (buffer1->arr[i1] != buffer2->arr[i2])
      return false;
    i1 = (i1+1) % buffer1->size;
    i2 = (i2+1) % buffer2->size;
  }
  return true;
}

void delete_buffer(Buffer* buffer) {
  free(buffer->arr);
  free(buffer);
}
