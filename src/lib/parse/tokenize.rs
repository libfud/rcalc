//! Tokenizes strings.

pub type MaybeToken<T, U> = (Option<Result<T, U>>, uint);

pub struct TokenStream<T, U> {
    expr: String,
    index: uint,
    rules: Vec<fn(&str) -> MaybeToken<T, U>>,
    on_err: U,
}

impl<T, U: Clone> TokenStream<T, U> {
    pub fn new(e: String, rules: Vec<fn(&str) -> MaybeToken<T, U>>,
               on_err: U) -> TokenStream<T, U> {
        TokenStream { expr: e, index: 0, rules: rules, /*tokens: tokens, */on_err: on_err }
    }

    pub fn peek(&self) -> Option<Result<T, U>> {
        self.peek_helper(0)
    }

    fn peek_helper(&self, j: uint) -> Option<Result<T, U>> {
        if self.index + j == self.expr.len() {
            return None
        } else {
            if self.expr.as_slice().slice_from(self.index).chars().next().unwrap().is_whitespace() {
                self.peek_helper(j + 1)
            } else {
                let (token, _) = analyze(self.expr.as_slice().slice_from(self.index + j), 
                                         self.rules.as_slice(), &self.on_err);
                token
            }
        }
    }

    pub fn rev(&mut self, i: uint) -> Result<(), ()> {
        if self.index >= i {
            self.index -= 1;
            Ok(())
        } else {
            Err(())
        }
    }

    pub fn expr(&self) -> String {
        self.expr.clone()
    }

    pub fn rules<'a>(&'a self) -> &'a [fn(&str) -> MaybeToken<T, U>] {
        self.rules.as_slice()
    }

    pub fn index(&self) -> uint {
        self.index
    }

    pub fn on_err(&self) -> U {
        self.on_err.clone()
    }
}

impl<T, U: Clone> Iterator<Result<T, U>> for TokenStream<T, U> {
    fn next(&mut self) -> Option<Result<T, U>> {
        if self.index == self.expr.len() {
            return None
        } else {
            if self.expr.as_slice().slice_from(self.index).chars().next().unwrap().is_whitespace() {
                self.index += 1;
                self.next()
            } else {
                let (token, len) = analyze(
                    self.expr.as_slice().slice_from(self.index), 
                    self.rules.as_slice(), &self.on_err);                  
                self.index += len;
                token
            }
        }
    }

    //returns the lowest amount of possible remaining tokens,
    //and the most possible remaining tokens
    fn size_hint(&self) -> (uint, Option<uint>) {
        if self.index == self.expr.len() {
            (0, None)
        } else {
            (1, Some(self.expr.len() - self.index))
        }
    }
}

pub fn analyze<T, U: Clone>(expr: &str, funs: &[fn(&str) -> MaybeToken<T, U>], 
                            on_err: &U) -> MaybeToken<T, U> {

    for &fun in funs.iter() {
        let (token, len) = fun(expr);
        if token.is_some() {
            return (token, len)
        }
    }

    (Some(Err(on_err.clone())), 0)
}
