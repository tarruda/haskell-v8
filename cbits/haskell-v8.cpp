#include <cstring>
#include <cstdlib>
#include <cstdio>
#include <v8.h>

using namespace v8;


extern "C" {
char * hello() {
  Isolate* isolate = Isolate::New();

  // Sets this isolate as the entered one for the current thread
  isolate->Enter();

  // Create a stack-allocated handle scope.
  HandleScope handle_scope(isolate);

  // Create a new context.
  Handle<Context> context = Context::New(isolate);

  // Here's how you could create a Persistent handle to the context, if needed.
  Persistent<Context> persistent_context(isolate, context);

  // Enter the created context for compiling and
  // running the hello world script. 
  Context::Scope context_scope(context);

  // Create a string containing the JavaScript source code.
  Handle<String> source = String::New("'Hello' + ', World!'");

  // Compile the source code.
  Handle<Script> script = Script::Compile(source);

  // Run the script to get the result.
  Handle<Value> result = script->Run();

  // The persistent handle needs to be eventually disposed.
  persistent_context.Dispose();

  // Convert the result to an ASCII string and print it.
  String::AsciiValue ascii(result);

  char *rv = (char *)malloc(strlen(*ascii) + 1);
  strcpy(rv, *ascii);

  return rv;
}
}
