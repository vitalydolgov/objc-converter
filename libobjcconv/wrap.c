#include <string.h>
#include <caml/mlvalues.h>
#include <caml/callback.h>
#include <caml/alloc.h>

char * process(char const *str)
{
  static const value *process_closure = NULL;
  if (process_closure == NULL) process_closure = caml_named_value("process");
  value str_value = caml_copy_string(str);
  char *result = strdup(String_val(caml_callback(*process_closure, str_value)));
  return result;
}
