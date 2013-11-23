#include <cstring>
#include <cstdlib>
#include <cstdio>

#include "haskell-v8.h"


Isolate *isolate = NULL;

V8Handle<Context> * new_context() {
  if (isolate == NULL) {
    isolate = Isolate::New();
  }

  isolate->Enter();
  HandleScope handle_scope(isolate);

  return new V8Handle<Context>(isolate, Context::New(isolate));
}

V8Handle<Value> * eval_in_context(char *str, V8Handle<Context> *c) {
  HandleScope handle_scope(isolate);
  Local<Context> context = Local<Context>::New(isolate, c->handle);
  Context::Scope context_scope(context);
  Handle<String> source = String::New(str);
  Handle<Script> script = Script::Compile(source);
  Handle<Value> result = script->Run();

  return new V8Handle<Value>(isolate, result);
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
