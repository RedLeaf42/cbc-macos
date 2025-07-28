// stdarg.hb

typedef unsigned long va_arg_t;
typedef void* va_list;

extern va_list va_init(void* arg);
extern va_arg_t va_next(va_list* ap);
