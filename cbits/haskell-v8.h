#ifndef __HASKELL_V8_
#define __HASKELL_V8_ value

#include <v8.h>

using namespace v8;


template <class T> class V8Handle {
public:
  V8Handle(Isolate *isolate, Handle<T> context) {
    handle.Reset(isolate, context);
  }

  ~V8Handle() {
    handle.Dispose();
  }

  Persistent<T> handle;
};

#ifdef __cplusplus
extern "C" {
#endif

extern V8Handle<Context> * new_context();
extern V8Handle<Value> * eval_in_context(char *str,
    V8Handle<Context> *context);

#ifdef __cplusplus
}
#endif

#endif
