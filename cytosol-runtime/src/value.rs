pub type RecordFields = Vec<Value>;

#[derive(Debug, Clone)]
pub enum Value {
    Bool(bool),
    Integer(isize),
    String(String),
    Record(RecordFields),
}

pub trait FromValue<'val>
where
    Self: 'val,
{
    fn from_value(val: &'val Value) -> Self;
}

impl<'a> FromValue<'a> for Value {
    fn from_value(val: &'a Value) -> Self {
        val.clone()
    }
}

impl<'a> FromValue<'a> for usize {
    fn from_value(val: &'a Value) -> Self {
        if let Value::Integer(i) = val {
            *i as usize
        } else {
            panic!("`usize::from_value()` on unexpected value {:?}", val)
        }
    }
}

impl<'a> FromValue<'a> for isize {
    fn from_value(val: &'a Value) -> Self {
        if let Value::Integer(i) = val {
            *i
        } else {
            panic!("`isize::from_value()` on unexpected value {:?}", val)
        }
    }
}

impl<'a> FromValue<'a> for String {
    fn from_value(val: &'a Value) -> String {
        if let Value::String(s) = val {
            s.clone()
        } else {
            panic!("`&str::from_value()` on unexpected value {:?}", val)
        }
    }
}

impl<'a> FromValue<'a> for Vec<Value> {
    fn from_value(val: &'a Value) -> Vec<Value> {
        if let Value::Record(fields) = val {
            fields.clone()
        } else {
            panic!("`&[Value]::from_value()` on unexpected value {:?}", val)
        }
    }
}

macro_rules! from_value_tuple {
    ($($t:ident,)*) => {
        impl<'a, $($t),*> FromValue<'a> for ($($t,)*)
        where
            $($t: FromValue<'a> + 'a),*,
        {
            #[allow(dead_code, unused_assignments)]
            fn from_value(val: &'a Value) -> ($($t,)*) {
                if let Value::Record(fields) = val {
                    let mut i = 0;

                    (
                        $($t::from_value(&fields[{let idx = i; i += 1; idx}]),)*
                    )
                } else {
                    let type_name = std::any::type_name::<($($t,)*)>();
                    panic!("`({})::from_value()` on unexpected value {:?}", type_name,val)
                }
            }
        }
    };
}

from_value_tuple!(A,);
from_value_tuple!(A, B,);
from_value_tuple!(A, B, C,);
from_value_tuple!(A, B, C, D,);
from_value_tuple!(A, B, C, D, E,);
from_value_tuple!(A, B, C, D, E, F,);
from_value_tuple!(A, B, C, D, E, F, G,);
from_value_tuple!(A, B, C, D, E, F, G, H,);

pub trait FromValueSlice<'a>
where
    Self: 'a,
{
    fn from_value_slice(vals: &'a [Value]) -> Self;
}

impl FromValueSlice<'_> for () {
    fn from_value_slice(_: &'_ [Value]) -> Self {}
}

macro_rules! from_value_slice {
    ($($t:ident,)*) => {
        impl<'a, $($t: 'a + for<'b> FromValue<'b>,)*> FromValueSlice<'a> for ($($t,)*)
        {
            #[allow(dead_code, unused_assignments)]
            fn from_value_slice(vals: &'a [Value]) -> ($($t,)*) {
                let mut i = 0;
                ($(
                    $t::from_value(&vals[{let idx = i; i += 1; idx}]),
                )*)
            }
        }
    };
}

from_value_slice!(A,);
from_value_slice!(A, B,);
from_value_slice!(A, B, C,);
from_value_slice!(A, B, C, D,);
from_value_slice!(A, B, C, D, E,);
from_value_slice!(A, B, C, D, E, F,);
from_value_slice!(A, B, C, D, E, F, G,);
from_value_slice!(A, B, C, D, E, F, G, H,);
