extern crate typemap;
extern crate sled;

use sled::{ConfigBuilder, Tree};
use typemap::Key;

pub struct SledMiddleware {
    pub tree: Tree
}

impl SledMiddleware {
    pub fn new() -> Result<SledMiddleware, Box<StdError>> {
        let config = ConfigBuilder::new().temporary(true).build();
        let tree = Tree::start(config).unwrap();
        
        Ok(SledMiddleware { tree: tree })
    }
}

impl Key for SledMiddleware { type Value = Tree }

impl<T> Middleware<T> for SledMiddleware {
    fn invoke<'mw, 'conn>(&self, req: &mut Request<'mw, 'conn, T>, res: Response<'mw, T>) -> MiddlewareResult<'mw, T> {
        req.extensions_mut().insert::<SledMiddleware>(self.tree.clone());
        Ok(Continue(res))
    }
}

pub trait SledRequestExtensions {
    fn sled_conn(&self) -> Tree;
}

impl<'a, 'b, T> SledRequestExtensions for Request<'a, 'b, T> {
    fn sled_conn(&self) -> Tree {
        self.extensions().get::<SledMiddleware>().unwrap().get().unwrap()
    }
}