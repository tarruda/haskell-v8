#include <cstring>
#include <cstdlib>
#include <cstdio>

#include "haskell-v8.h"


void free_isolate(Isolate *isolate) {
  isolate_data *data = get_isolate_data(isolate);

  if (data->ref_count == 0) {
    fprintf(stderr, "Disposing isolate\n");
    isolate->Dispose();
  } else {
    fprintf(stderr, "Cannot dispose isolate due to pending refs\n");
    data->should_dispose = 1;
  }
}


void free_context(V8Ref<Context> *handle) { delete handle; }
void free_value(V8Ref<Value> *handle) { delete handle; }


Isolate * new_isolate() {
  return Isolate::New();
}


V8Ref<Context> * new_context(Isolate *isolate) {
  Locker locker(isolate);
  Isolate::Scope isolate_scope(isolate);
  HandleScope handle_scope(isolate);

  return new V8Ref<Context>(isolate, Context::New(isolate));
}


V8Ref<Value> * eval_in_context(char *str, V8Ref<Context> *c) {
  Isolate *isolate = c->isolate;
  Locker locker(isolate);
  Isolate::Scope isolate_scope(isolate);
  HandleScope handle_scope(isolate);
  Local<Context> context = Local<Context>::New(isolate, c->handle);
  Context::Scope context_scope(context);
  Handle<String> source = String::New(str);
  Handle<Script> script = Script::Compile(source);
  Handle<Value> result = script->Run();

  return new V8Ref<Value>(isolate, result);
}
//
//char * hello() {
//  Isolate* isolate = isolate::New();
//
//  // sets this isolate as the entered one for the current thread
//  Isolate->Enter();
//
//  // Create a stack-allocated handle scope.
//  HandleScope handle_scope(isolate);
//
//  // Create a new context.
//  Handle<Context> context = Context::New(isolate);
//
//  // Here's how you could create a Persistent handle to the context, if needed.
//  Persistent<Context> persistent_context(isolate, context);
//
//  // Enter the created context for compiling and
//  // running the hello world script. 
//  Context::Scope context_scope(context);
//
//  // Create a string containing the JavaScript source code.
//  Handle<String> source = String::New("'Hello' + ', World!'");
//
//  // Compile the source code.
//  Handle<Script> script = Script::Compile(source);
//
//  // Run the script to get the result.
//  Handle<Value> result = script->Run();
//
//  // The persistent handle needs to be eventually disposed.
//  persistent_context.Dispose();
//
//  // Convert the result to an ASCII string and print it.
//  String::AsciiValue ascii(result);
//
//  char *rv = (char *)malloc(strlen(*ascii) + 1);
//  strcpy(rv, *ascii);
//
//  return rv;
//}
