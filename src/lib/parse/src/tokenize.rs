//! Tokenizes strings.

pub type MaybeToken<T, U> = (Option<Result<T, U>>, usize);

pub struct TokenStream<T, U> {
    expr: String,
    index: usize,
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

    fn peek_helper(&self, j: usize) -> Option<Result<T, U>> {
        if self.index + j == self.expr.len() {
            return None
        } else {
            if self.expr.as_slice().chars().skip(self.index).next().unwrap().is_whitespace() {
// if self.expr.as_slice().slice_from(self.index).chars().next().unwrap().is_whitespace() {
                self.peek_helper(j + 1)
            } else {
                let temp_string: String = 
                    self.expr.as_slice().chars().skip(self.index + j).collect();
                let (token, _) = analyze(temp_string.as_slice(),
                                         self.rules.as_slice(), &self.on_err);
                token
            }
        }
    }

    pub fn rev(&mut self, i: usize) -> Result<(), ()> {
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

    pub fn index(&self) -> usize {
        self.index
    }

    pub fn on_err(&self) -> U {
        self.on_err.clone()
    }
}

impl<T, U: Clone> Iterator for TokenStream<T, U> {
    type Item = Result<T, U>;

    fn next(&mut self) -> Option<Result<T, U>> {
        if self.index == self.expr.len() {
            return None
        } else {
            if self.expr.as_slice().chars().skip(self.index).next().unwrap().is_whitespace() {
// if self.expr.as_slice().slice_from(self.index).chars().next().unwrap().is_whitespace() {
// double check semantics
                self.index += 1;
                self.next()
            } else {
                let temp_string: String = self.expr.as_slice().chars().skip(self.index).collect();
                let (token, len) = analyze(
//                    self.expr.as_slice().slice_from(self.index), 
                    temp_string.as_slice(),
                    self.rules.as_slice(), &self.on_err);                  
                self.index += len;
                token
            }
        }
    }

    //returns the lowest amount of possible remaining tokens,
    //and the most possible remaining tokens
    fn size_hint(&self) -> (usize, Option<usize>) {
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
