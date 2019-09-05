/*
 * Copyright 2019 Google LLC
 *
 * Use of this source code is governed by a BSD-style
 * license that can be found in the LICENSE file or at
 * https://developers.google.com/open-source/licenses/bsd
 */

#include <stdbool.h>

typedef struct Buffer {
  int top;
  int bot;
  int* arr;
  int size;
} Buffer;


Buffer* new_buffer(int size);
void put_buffer(int i, Buffer* buffer);
int get_buffer(Buffer* buffer);
int len_buffer(Buffer* buffer);
bool same_elements(Buffer* buffer1, Buffer* buffer2);
void delete_buffer(Buffer* buffer);
