#include <cstdlib>

#include "haskell-v8.h"


void free_isolate(Isolate *isolate) {
  isolate_data *data = get_isolate_data(isolate);

  if (data->ref_count == 0) {
    isolate->Dispose();
  } else {
    data->should_dispose = 1;
  }
}


void free_context(V8Ref<Context> *handle) {
  if (handle->ref_count == 0) {
    delete handle;
  } else {
    handle->should_dispose = 1;
  }
}

void free_value(V8Object *handle) { delete handle; }


Isolate * new_isolate() {
  return Isolate::New();
}


V8Ref<Context> * new_context(Isolate *isolate) {
  Locker locker(isolate);
  Isolate::Scope isolate_scope(isolate);
  HandleScope handle_scope(isolate);

  return new V8Ref<Context>(isolate, Context::New(isolate));
}


v8_result * eval_in_context(char *str, V8Ref<Context> *c) {
  Isolate *isolate = c->isolate;
  Locker locker(isolate);
  Isolate::Scope isolate_scope(isolate);
  HandleScope handle_scope(isolate);
  Local<Context> context = Local<Context>::New(isolate, c->handle);
  Context::Scope context_scope(context);
  Handle<String> source = String::New(str);
  Handle<Value> result;

  v8_result *rv = (v8_result *)malloc(sizeof(v8_result));
  rv->thrown = 0;

  TryCatch tryCatch;
  Handle<Script> script = Script::Compile(source);

  if (script.IsEmpty()) {
    result = tryCatch.Exception();
    rv->thrown = 1;
  } else {
    result = script->Run();
    if (result.IsEmpty()) {
      result = tryCatch.Exception();
      rv->thrown = 1;
    }
  }

  rv->value = new V8Object(new V8Ref<Value>(isolate, result), c);

  if (result->IsUndefined())
    rv->type = V8TYPE_UNDEFINED;
  else if (result->IsNull())
    rv->type = V8TYPE_NULL;
  else if (result->IsTrue())
    rv->type = V8TYPE_TRUE;
  else if (result->IsFalse())
    rv->type = V8TYPE_FALSE;
  else if (result->IsNumber())
    rv->type = V8TYPE_NUMBER;
  else if (result->IsString())
    rv->type = V8TYPE_STRING;
  else if (result->IsArray())
    rv->type = V8TYPE_ARRAY;
  else if (result->IsRegExp())
    rv->type = V8TYPE_REGEXP;
  else if (result->IsDate())
    rv->type = V8TYPE_DATE;
  else
    rv->type = V8TYPE_OBJECT;

  return rv;
}


char * v8_to_string(V8Object *o) {
  Isolate *isolate = o->value->isolate;
  Locker locker(isolate);
  Isolate::Scope isolate_scope(isolate);
  HandleScope handle_scope(isolate);
  Local<Context> context = Local<Context>::New(isolate, o->context->handle);
  Context::Scope context_scope(context);
  Local<String> str = Local<Value>::New(isolate, o->value->handle)->ToString();
  char *rv = (char *)malloc(str->Utf8Length() + 1);
  str->WriteUtf8(rv);
  return rv;
}


