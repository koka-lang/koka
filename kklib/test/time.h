#pragma once

#include <stdint.h>

typedef int64_t msecs_t;

msecs_t _clock_start(void);
msecs_t _clock_end(msecs_t start);
