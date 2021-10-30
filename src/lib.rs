type Id = i64;

// * Type

trait Type {
    fn type_id(self) -> Id;
    fn read_data (self) -> ();
}

trait Link {

}

// ** The Book type

struct Book {

}

// * Thing

pub struct Thing {
    id: Id,
    pub name: String,
    typ: dyn Type,
}

// * Store

pub struct Store {
    things: ()
}
