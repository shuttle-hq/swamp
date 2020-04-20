use sha2::{digest, Digest, Sha256};

pub trait Compress {
    fn digest<I: digest::Input>(&self, hasher: &mut I);
    fn compress(&self) -> [u8; 32] {
        let mut hasher = sha2::Sha256::new();
        self.digest(&mut hasher);
        hasher.result().into()
    }
}

macro_rules! impl_compress_for_primitive {
    ($p:ident) => {
        impl Compress for $p {
            fn digest<I: digest::Input>(&self, hasher: &mut I) {
                hasher.input(self.to_ne_bytes());
            }
        }
    };
}

impl_compress_for_primitive!(i32);
impl_compress_for_primitive!(u32);

impl<T: Compress> Compress for Option<T> {
    fn digest<I: digest::Input>(&self, hasher: &mut I) {
        if let Some(t) = self.as_ref() {
            t.digest(hasher);
        }
    }
}

impl Compress for String {
    fn digest<I: digest::Input>(&self, hasher: &mut I) {
        hasher.input(self.as_bytes())
    }
}

impl<T: Compress> Compress for Vec<T> {
    fn digest<I: digest::Input>(&self, hasher: &mut I) {
        for token in self.iter() {
            token.digest(hasher);
        }
    }
}
