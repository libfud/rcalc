use super::{Matrice, MatrixResult, BadDimensionality};

#[test]
fn matrix_empty_test() {
    let empty: Matrice<int> = Matrice::new_empty();
    assert!(empty == Matrice { columns: 0, rows: 0, elems: vec!() });
}

#[test]
fn matrix_new_test() {
    let x = Matrice::new(vec!(1i, 2, 3, 4), 2, 2);
    assert!(x == Ok(Matrice { columns: 2, rows: 2, elems: vec!(1, 2, 3, 4) }));

    let y: MatrixResult<Matrice<int>> = Matrice::new(vec!(), 1, 1);
    assert!(y == Err(BadDimensionality));
}

#[test]
fn matrix_get_row_test() {
    let fake_vec: Vec<int> = range(1, 5).collect();
    let x = match Matrice::new(fake_vec, 2, 2) {
        Ok(x) => x,
        Err(_) => fail!("Failed test.")
    };

    let row_0: Vec<int> = x.get_row(0).map(|x| x.clone()).collect();
    assert!(row_0 == vec!(1, 2));

    let row_1: Vec<int> = x.get_row(1).map(|x| x.clone()).collect();
    assert!(row_1 == vec!(3, 4));

    let fake_vec: Vec<int> = range(1, 11).collect();
    let z = match Matrice::new(fake_vec, 2, 5) {
        Ok(x) => x,
        _ => fail!("wut")
    };

    let row_4: Vec<int> = z.get_row(4).map(|x| x.clone()).collect();
    assert!(row_4 == vec!(9, 10));
}

#[test]
fn matrix_get_col_test() {
    let fake_vec: Vec<int> = range(1, 7).collect();
    
    let x = match Matrice::new(fake_vec.clone(), 2, 3) {
        Ok(x) => x,
        Err(_) => fail!("Failed test.")
    };
    
    let col_0: Vec<int> = x.get_col(0).map(|x| x.clone()).collect();
    assert!(col_0 == vec!(1, 3, 5));

    let col_1: Vec<int> = x.get_col(1).map(|x| x.clone()).collect();
    assert!(col_1 == vec!(2, 4, 6));

    let fake_vec: Vec<int> = range(1, 13).collect();
    let z = match Matrice::new(fake_vec, 3, 4) {
        Ok(x) => x,
        _ => fail!("wut")
    };

    let zcol1: Vec<int> = z.get_col(1).map(|x| x.clone()).collect();
    assert!(zcol1 == vec!(2, 5, 8, 11));
}

#[test]
fn test_append_col() {
    let x: Matrice<int> = Matrice { columns: 3, rows: 3, elems: vec!(6,  1, 1,
                                                                     4, -2, 5,
                                                                     2, 8, 7)};
    let y = x.submatrix(0, 1, 2, 1).unwrap();
    let z = x.submatrix(2, 1, 2, 1).unwrap();
    assert!(y.concat_cols(&z) == Some( Matrice { columns: 2, rows: 2 , elems: vec!(4, 5,
                                                                                  2, 7)}));
}

#[test]
fn submatrix_test() {
    let x: Matrice<int> = Matrice::ident(5);
    /* (1, 0, 0, 0, 0) *
     * (0, 1, 0, 0, 0) *
     * (0, 0, 1, 0, 0) *
     * (0, 0, 0, 1, 0) *
     * (0, 0, 0, 0, 1) */

    assert!(x.submatrix(0, 0, 1, 1) == Some(Matrice { rows: 1, columns: 1,
                                                     elems: vec!(1) }));
    assert!(x.submatrix(0, 0, 1, 2) == Some(Matrice { rows: 1, columns: 2,
                                                      elems: vec!(1, 0)}));
    assert!(x.submatrix(0, 0, 1, 5) == Some(Matrice { rows: 1, columns: 5,
                                                      elems: vec!(1, 0, 0, 0, 0)}));
    assert!(x.submatrix(0, 1, 1, 5) == Some(Matrice { rows: 1, columns: 5,
                                                      elems: vec!(0, 1, 0, 0, 0)}));
    assert!(x.submatrix(1, 1, 1, 4) == Some(Matrice { rows: 1, columns: 4,
                                                      elems: vec!(1, 0, 0, 0)}));
    assert!(x.submatrix(1, 1, 3, 3) == Some(Matrice { rows: 3, columns: 3,
                                                      elems: vec!(1, 0, 0,
                                                                  0, 1, 0,
                                                                  0, 0, 1)}));
}

#[test]
fn ident_test() {
    let mut x: Matrice<int> = Matrice::ident(1);
    assert!(x == Matrice { rows: 1, columns: 1, elems: vec!(1i) });

    x = Matrice::ident(2);
    assert!(x == Matrice { rows: 2, columns: 2, elems: vec!(1i, 0, 0, 1) });

    x = Matrice::ident(3);
    assert!(x == Matrice { rows: 3, columns: 3, elems: vec!(1, 0, 0, 
                                                            0, 1, 0,
                                                            0, 0, 1) });
    x = Matrice::ident(4);
    assert!(x == Matrice { rows: 4, columns: 4, elems: vec!(1, 0, 0, 0,
                                                            0, 1, 0, 0,
                                                            0, 0, 1, 0,
                                                            0, 0, 0, 1) });
}

#[test]
fn deterimant_test() {
    let a: Matrice<int> = Matrice { columns: 2, rows: 2, elems: vec!(4, 6, 3, 8) };
    assert!(a.determinant() == Some(14));

    let b: Matrice<int> = Matrice { columns: 2, rows: 3, elems: vec!(5, 7, 9, 11, 13, 15) };
    assert!(b.determinant() == None);
    
    let x: Matrice<int> = Matrice { columns: 3, rows: 3, elems: vec!(6,  1, 1,
                                                                     4, -2, 5,
                                                                     2,  8, 7)};

    fail!(x.determinant().to_str())
    //assert!(x.determinant() == Some(-306));
}

#[test]
fn test_add() {
    let lhs = match Matrice::new(vec!(1i, 2, 3, 4), 2, 2) {
        Ok(x) => x,
        Err(m) => fail!(m.to_str())
    };
    let rhs = match Matrice::new(vec!(5i, 6i, 7i, 8i), 2, 2) {
        Ok(x) => x,
        Err(m) => fail!(m.to_str())
    };

    assert!(lhs + rhs == Matrice { columns: 2, rows: 2, elems: vec!(6i, 8i, 10i, 12i) });
}

#[test]
fn test_sub() {
    let lhs = match Matrice::new(vec!(1i, 2, 3, 4), 2, 2) {
        Ok(x) => x,
        Err(m) => fail!(m.to_str())
    };
    let rhs = match Matrice::new(vec!(5i, 6i, 7i, 8i), 2, 2) {
        Ok(x) => x,
        Err(m) => fail!(m.to_str())
    };

    assert!(lhs - rhs == Matrice { columns: 2, rows: 2, elems: vec!(-4i, -4i, -4i, -4i) });
}

#[test]
fn test_mul() {
    let lhs = match Matrice::new(vec!(1i, 2, 3, 4), 2, 2) {
        Ok(x) => x,
        Err(m) => fail!(m.to_str())
    };
    let rhs = match Matrice::new(vec!(5i, 6i, 7i, 8i), 2, 2) {
        Ok(x) => x,
        Err(m) => fail!(m.to_str())
    };

    let results = vec!((1i * 5 + 2 * 7), (1 * 6 + 2 * 8),
                       (3 * 5 + 4 * 7), (3 * 6 + 4 * 8));

    assert!(lhs * rhs == Matrice { columns: 2, rows: 2, elems: results });
}
