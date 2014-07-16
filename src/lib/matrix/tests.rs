extern crate num;

use super::{Matrice, MatrixResult, BadDimensionality};
use self::num::rational::BigRational;
use std::num;
use std::f64;


#[test]
fn matrix_empty_test() {
    let empty: Matrice<int> = Matrice::new();
    assert!(empty == Matrice { columns: 0, rows: 0, elems: vec!() });
}

#[test]
fn matrix_new_test() {
    let x = Matrice::from_vec(vec!(1i, 2, 3, 4), 2, 2);
    assert!(x == Ok(Matrice { columns: 2, rows: 2, elems: vec!(1, 2, 3, 4) }));

    let y: MatrixResult<Matrice<int>> = Matrice::from_vec(vec!(), 1, 1);
    assert!(y == Err(BadDimensionality));
}


#[test]
fn matrix_get_row_test() {
    let fake_vec: Vec<int> = range(1, 5).collect();
    let x = match Matrice::from_vec(fake_vec, 2, 2) {
        Ok(x) => x,
        Err(_) => fail!("Failed test.")
    };

    let row_0: Vec<int> = x.get_row(0).map(|x| x.clone()).collect();
    assert!(row_0 == vec!(1, 2));

    let row_1: Vec<int> = x.get_row(1).map(|x| x.clone()).collect();
    assert!(row_1 == vec!(3, 4));

    let fake_vec: Vec<int> = range(1, 11).collect();
    let z = match Matrice::from_vec(fake_vec, 2, 5) {
        Ok(x) => x,
        _ => fail!("wut")
    };

    let row_4: Vec<int> = z.get_row(4).map(|x| x.clone()).collect();
    assert!(row_4 == vec!(9, 10));
}

#[test]
fn matrix_get_col_test() {
    let fake_vec: Vec<int> = range(1, 7).collect();
    
    let x = match Matrice::from_vec(fake_vec.clone(), 2, 3) {
        Ok(x) => x,
        Err(_) => fail!("Failed test.")
    };
    
    let col_0: Vec<int> = x.get_col(0).map(|x| x.clone()).collect();
    assert!(col_0 == vec!(1i, 3, 5));

    let col_1: Vec<int> = x.get_col(1).map(|x| x.clone()).collect();
    assert!(col_1 == vec!(2i, 4, 6));

    let fake_vec: Vec<int> = range(1, 13).collect();
    let z = match Matrice::from_vec(fake_vec, 3, 4) {
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
fn concat_cols_test() {
    let a: Matrice<int> = Matrice { columns: 1, rows: 2, elems: vec!(4, 2) };
    let b: Matrice<int> = Matrice { columns: 1, rows: 2, elems: vec!(5, 7) };

    assert!(a.concat_cols(&b) == Some(Matrice { columns: 2, rows: 2,
                                                elems: vec!(4, 5,
                                                            2, 7)}));
}

/*
#[test] 
fn gauss() {
    let x: Matrice<f64> = Matrice { columns: 3, rows: 3, elems: vec!( 25.,  5., 1.,
                                                                      64.,  8., 1.,
                                                                     144., 12., 1.)};
    //I despise f64
    let z: Vec<f64> = vec!(25f64, 5f64,  1f64,
                           0f64,  -4.80000000000000071054, -1.560000000000000005329,
                           0f64,  0f64,   0.69999999999999928946);

    assert_eq!(x.gauss_xform(), Some(Matrice { columns: 3, rows: 3,
                                               elems: z}));
}
*/

#[test]
fn determinant_test() {
    use std::f64;
    use std::num;

    let id: Matrice<f64> = Matrice::ident(11);
    assert_eq!(id.determinant(), Some(1.));

    let x: Matrice<f64> = Matrice { columns: 3, rows: 3, elems: vec!( 25.,  5., 1.,
                                                                      64.,  8., 1.,
                                                                     144., 12., 1.)};

    assert_eq!(f64::to_str_digits(x.determinant().unwrap(), 20),
               f64::to_str_digits(-83.99999999999992894573, 20));

    let c: Matrice<int> = Matrice { columns: 3, rows: 3, elems: vec!(1, 2, 3,
                                                                     4, 5, 6,
                                                                     7, 8, 9)};

    assert_eq!(c.determinant(), Some(0));
     
    
    let x: Matrice<f64> = Matrice { columns: 3, rows: 3, elems: vec!(6.,  1., 1.,
                                                                     4., -2., 5.,
                                                                     2.,  8., 7.)};
    assert_eq!(x.determinant(), Some(-306.));


    let zero: BigRational = num::zero();
    let one: BigRational = num::one();
    let two: BigRational = one + num::one(); 
    let three: BigRational = two + one;
    let four: BigRational = two + two;
    let six: BigRational = two * three;

    let bigrat_vec = vec!(two + two, four + one, three * two, four + three,
                          one + two,  one + one,  zero + one,  two + three,
                          six + one,  six + two, six + three,    two + two,
                          one + one,  one + one,   one + two,   one + zero);

    let d: Matrice<BigRational> = Matrice { columns: 4, rows: 4, elems: bigrat_vec };
    assert_eq!(d.determinant(), from_str::<BigRational>("-57/1"));


    let y: Matrice<f64> = Matrice { columns: 4, rows: 4, elems: vec!( 1.,  2.,  3.,  4.,
                                                                      5.,  6.,  7.,  8.,
                                                                      9., 10., 11., 12.,
                                                                     13., 14., 15., 16.)};
    assert_eq!(y.determinant(), Some(0.));


    let z: Matrice<f64> = Matrice { columns: 4, rows: 4, elems: vec!(  6.,   1.,   1.,   9.,
                                                                       4.,  -2.,   5.,  22.,
                                                                       2.,   8.,   7.,  11.,
                                                                     -13., -17., -15., 100.)};
    assert_eq!(f64::to_str_digits(z.determinant().unwrap(), 20),
               f64::to_str_digits(-41247.00000000000727595761, 20));

    let r: Matrice<f64> = Matrice { columns: 5, rows: 5, elems: vec!(  1.,  0., 0.,  0.,  0.,
                                                                       0.,  1., 0.,  0., -1.,
                                                                       0.,  3., 0., -3., -2.,
                                                                       0.,  0., 2., -1.,  0.,
                                                                       1., -1., 0.,  1.,  0.)};
    assert_eq!(r.determinant(), Some(-4.));

    /*  1,  0,  0,  1, -2,  1,
     *  0, -2,  1,  0,  0,  2,
     * -1, -3,  0,  0,  1, -3,
     * -4,  2,  1,  0,  0,  0,
     *  0,  0,  2,  1,  1,  1,
     * -1,  5, -1, -1,  0,  0)*/

    let bigrat_vec = vec!(one + zero, zero * one, zero * one,  one * one,       -two, one * one,
                          //       1,          0,          0,          1,         -2,         1,
                          zero * one,       -two,  one * one, zero * one, zero * one, two * one,
                          //       0,         -2,          1,          0,          0,         2,
                                -one,     -three, zero * one, zero * one,  one * one,    -three,
                          //      -1,         -3,          0,          0,          1,        -3,
                               -four,  two * one,  one * one, zero * one, zero * one,  zero*one,
                          //      -4,          2,          1,          0,          0,         0,
                          zero * one, zero * one,  one + one,  one * one,  one * one, one * one,
                          //       0,          0,          2,          1,          1,         1,
                                -one, four + one,       -one,       -one, zero * one,  zero*one);
                          //      -1,          5,         -1,         -1,          0,         0

    let q: Matrice<BigRational> = Matrice { columns: 6, rows: 6, elems: bigrat_vec };


    let five = four + one;
    let fourty = (five * two) * four;
    assert_eq!(q.determinant(), Some(-fourty));

}

#[test]
fn transpose_test() {
    let x: Matrice<int>  = Matrice { columns: 3, rows: 2, elems: vec!(1, 3, 7,
                                                                      9, 4, 6)};
    assert_eq!(x.transpose(), Matrice { columns: 2, rows: 3, elems: vec!(1, 9,
                                                                         3, 4,
                                                                         7, 6)});
}

#[test]
fn inverse_test() {

    let x: Matrice<f64> = Matrice { columns: 2, rows: 2, elems: vec!(1., 2., 3., 4.)};
    assert_eq!(x.inverse(), Some(Matrice { columns: 2, rows: 2, 
                                           elems: vec!(-2., 1., 1.5, -0.5)}));
    let y: Matrice<f64> = Matrice { columns: 3, rows: 3, elems: vec!(1., 2., 3.,
                                                                     5., 4., 6.,
                                                                     7., 8., 9.)};
    assert_eq!(y.inverse(), Some(Matrice { columns: 3, rows: 3,
                                           elems: vec!(-4./6.,  2./6.,     0.,
                                                       -1./6., -4./6.,  3./6.,
                                                        4./6.,  2./6., -2./6.)}));
    assert_eq!(y.determinant(), Some(18.));
    assert_eq!(y.inverse().unwrap().determinant(), Some(1./18.)); 

    let z: Matrice<f64> = Matrice::ident(150);
    assert_eq!(z.inverse().unwrap(), z);

    /* http://www.wolframalpha.com/input/?i=inverse{{1,0,2,-1,0,1,3,1},
     * {-1,0,-1,1,-4,3,2,1},{0,0,0,1,7,-4,-1,-1},{0,0,3,4,9,1,-1,-2},
     * {0,1,0,1,1,-1,1,2},{-3,1,-2,4,-5,-1,0,1},{1,1,0,0,1,0,-1,-1},
     * {1,0,1,0,1,1,3,4}} */

    let f64_vec: Vec<f64> = vec!( 1., 0.,  2., -1.,  0.,  1.,  3.,  1.,
                                 -1., 0., -1.,  1., -4.,  3.,  2.,  1.,
                                  0., 0.,  0.,  1.,  7., -4., -1., -1.,
                                  0., 0.,  3.,  4.,  9.,  1., -1., -2.,
                                  0., 1.,  0.,  1.,  1., -1.,  1.,  2.,
                                 -3., 1., -2.,  4., -5., -1.,  0.,  1.,
                                  1., 1.,  0.,  0.,  1.,  0., -1., -1.,
                                  1., 0.,  1.,  0.,  1.,  1.,  3.,  4.);


    let mut results: Vec<f64> = 
        vec!( 452.,  -766.,  1310., -478., -5876.,  1924.,  3952.,  3612.,
               76.,   657., -1045.,  186.,  5032., -1328.,  -694., -2709.,
              628., -1304., -1110.,  428.,   866.,   116.,  -982.,  -602.,
              272.,  -421.,   775.,   32., -3536.,  1584.,  1952.,  2107.,
             -352.,   536.,    50.,  138.,  1306.,  -694.,  -612.,  -602.,
             -562.,   726.,  -550.,  288.,  1286.,  -794.,  -492.,  -602.,
              970.,   900.,   960., -240.,  -570.,   160.,   410.,     0.,
             -774.,  -473.,  -645.,   86.,  1032.,  -258.,  -774.,   301.
        );
    let determinant = 3010.;
    results = results.iter().map(|x| *x / determinant).collect();

    let wow = Matrice { columns: 8, rows: 8, elems: f64_vec };
    let ugh = Matrice { columns: 8, rows: 8, elems: results };
    let epsilon: f64 = 0.0075;

    assert!((wow.determinant().unwrap() + determinant).abs() < epsilon);

    let inv = wow.inverse().unwrap();

    for (a, b) in inv.elems.iter().zip(ugh.elems.iter()) {
        if (a - *b).abs() > epsilon {
            fail!(format!("Difference of {} and {} = {}, epsilon: {}", 
                          f64::to_str_digits(*a, 20),
                          f64::to_str_digits(*b, 20),
                          f64::to_str_digits(a - *b, 20),
                          f64::to_str_digits(epsilon, 20)))
        }
    }
} 


#[test]
fn sans_test() {
    let x: Matrice<int> = Matrice { columns: 4, rows: 4, elems: vec!( 1,  2,  3,  4,
                                                                      5,  6,  7,  8,
                                                                      9, 10, 11, 12,
                                                                     13, 14, 15, 16) };

    let ints_vec: Vec<int> = range(1, 17).map(|x| x).collect();
    let ref_ints: Vec<&int> = ints_vec.iter().map(|x| x).collect();

    assert_eq!(x.sans_row_col(0, 0), Matrice { columns: 4, rows: 4, elems: ref_ints.clone()});
    assert_eq!(x.sans_row_col(5, 5), Matrice { columns: 4, rows: 4, elems: ref_ints});

    let ints_vec = vec!( 5,  6,  7,  8,
                         9, 10, 11, 12,
                        13, 14, 15, 16);
    let ref_ints: Vec<&int> = ints_vec.iter().map(|x| x).collect();

    assert_eq!(x.sans_row_col(1, 0), Matrice { columns: 4, rows: 3, elems: ref_ints});

    let ints_vec = vec!( 2,  3,  4,
                         6,  7,  8,
                        10, 11, 12,
                        14, 15, 16);
    let ref_ints: Vec<&int> = ints_vec.iter().map(|x| x).collect();

    assert_eq!(x.sans_row_col(0, 1), Matrice { columns: 3, rows: 4, elems: ref_ints});

    let ints_vec = vec!( 1,  3,  4,
                         9, 11, 12,
                        13, 15, 16);
    let ref_ints: Vec<&int> = ints_vec.iter().map(|x| x).collect();

    assert_eq!(x.sans_row_col(2, 2), Matrice { columns: 3, rows: 3, elems: ref_ints});

    let ints_vec = vec!(1,  2,  3,
                        5,  6,  7,
                        9, 10, 11);
    let ref_ints: Vec<&int> = ints_vec.iter().map(|x| x).collect();
    
    assert_eq!(x.sans_row_col(4, 4), Matrice { columns: 3, rows: 3, elems: ref_ints});
}

#[test]
fn test_add() {
    let lhs = match Matrice::from_vec(vec!(1i, 2, 3, 4), 2, 2) {
        Ok(x) => x,
        Err(m) => fail!(m.to_string())
    };
    let rhs = match Matrice::from_vec(vec!(5i, 6i, 7i, 8i), 2, 2) {
        Ok(x) => x,
        Err(m) => fail!(m.to_string())
    };

    assert!(lhs + rhs == Matrice { columns: 2, rows: 2, elems: vec!(6i, 8i, 10i, 12i) });
}

#[test]
fn test_sub() {
    let lhs = match Matrice::from_vec(vec!(1i, 2, 3, 4), 2, 2) {
        Ok(x) => x,
        Err(m) => fail!(m.to_string())
    };
    let rhs = match Matrice::from_vec(vec!(5i, 6i, 7i, 8i), 2, 2) {
        Ok(x) => x,
        Err(m) => fail!(m.to_string())
    };

    assert!(lhs - rhs == Matrice { columns: 2, rows: 2, elems: vec!(-4i, -4i, -4i, -4i) });
}

#[test]
fn test_mul() {
    let lhs = match Matrice::from_vec(vec!(1i, 2, 3, 4), 2, 2) {
        Ok(x) => x,
        Err(m) => fail!(m.to_string())
    };
    let rhs = match Matrice::from_vec(vec!(5i, 6i, 7i, 8i), 2, 2) {
        Ok(x) => x,
        Err(m) => fail!(m.to_string())
    };

    let results = vec!((1i * 5 + 2 * 7), (1 * 6 + 2 * 8),
                       (3 * 5 + 4 * 7), (3 * 6 + 4 * 8));

    assert!(lhs * rhs == Matrice { columns: 2, rows: 2, elems: results });
}

/*
#[test]
fn test_div() {
    let lhs: Matrice<f64> = Matrice { columns: 2, rows: 2, elems: vec!(1., 2., 3., 4.)};
    let rhs: Matrice<f64> = Matrice { columns: 2, rows: 2, elems: vec!(9., 8., 7., 4.)};

    let res = lhs / rhs;
    assert_eq!(res, Matrice { columns: 2, rows: 2, elems:
                                   vec!(5./10., -5./10., 8./10., -6./10.)});
}
*/
