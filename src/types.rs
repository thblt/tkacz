/*!
The type system for structured Things.

```
type Name
 = CommonName
      { pre: Text
      , first: Text
      , middle: Text
      , von: Text
      , last: Text
      , suffix: Text }
  | SimpleName Text
```
 */

use std::collections::LinkedList;

type Identifier<'a> = &'a str;

// * Types

// ** Primitive types

pub enum Type<'a> {
    String,
    Int,
    Float,
    Bool,
    And { a: &'a Type<'a>, b: &'a Type<'a> },
    Or { either: &'a Type<'a>, or: &'a Type<'a> },
    List { t: &'a Type<'a> },
    Class { fields: Class<'a> },
    Link { t:  &'a Type<'a> },
    Nil
}

pub enum Value<'a> {
    Text { v: String },
    Int { v: i64},
    Float { v: f64 },
    Bool { v: bool },
    And { a: &'a Object<'a>, b: &'a Object<'a> },
    Or { v: &'a Object<'a>, first: bool },
    List { first: &'a Object<'a>, rest: &'a Object<'a>},
    Object { class: Identifier<'a>, fields: Object<'a> },
    Void
}

pub type Class<'a> = LinkedList<NamedType<'a, >>;
pub type Object<'a> = LinkedList<NamedValue<'a, >>;

/// Something with a name.
pub struct Named<'a, V> {
    k: Identifier<'a>,
    v: V
}

pub type NamedValue<'a> = Named<'a, Value<'a>>;
pub type NamedType<'a> = Named<'a, Type<'a>>;

// * Simple From<?> implementations

impl From<i64> for Value<'_> {
    fn from(v: i64) -> Self {
        Value::Int { v }
    }
}

impl From<f64> for Value<'_> {
    fn from(v: f64) -> Self {
        Value::Float { v }
    }
}

impl From<bool> for Value<'_> {
    fn from(v: bool) -> Self {
        Value::Bool { v }
    }
}

impl From<String> for Value<'_> {
    fn from(v: String) -> Self {
        Value::Text { v }
    }
}
