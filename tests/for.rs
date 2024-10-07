use loxr::Value;
use loxr::Vm;

#[test]
fn endless_for() {
    let mut vm = Vm::test();
    let source_code = "for(;;) { print 1; }";
    vm.compile(&source_code).unwrap();
    println!("{}", vm.code());
    let _ = vm.run_exact(100).unwrap(); // Should be enough to confirm it never breaks out of the loop

    let result = vm.read_out();
    let lines = result.split_ascii_whitespace();
    for line in lines {
        assert_eq!(line, "1");
    }
}

#[test]
fn endless_for_with_var() {
    let mut vm = Vm::test();
    let source_code = "for(var i = 2;;) { print i; }";
    vm.compile(&source_code).unwrap();
    let _ = vm.run_exact(100).unwrap(); // Should be enough to confirm it never breaks out of the loop

    let result = vm.read_out();
    let lines = result.split_ascii_whitespace();
    for line in lines {
        assert_eq!(line, "2");
    }
}

#[test]
fn endless_for_with_incr_var() {
    let mut vm = Vm::test();
    let source_code = "for (var i = 0;; i = i + 1) { print i; }";
    vm.compile(&source_code).unwrap();
    let _ = vm.run_exact(100).unwrap(); // Should be enough to confirm it never breaks out of the loop

    let result = vm.read_out();
    let lines = result.split_ascii_whitespace();
    for (index, line) in lines.enumerate() {
        assert_eq!(line, format!("{index}"));
    }
}

#[test]
fn for_always_true() {
    let mut vm = Vm::test();
    let source_code = "for (var i = 0; true;) { print i; }";
    vm.compile(&source_code).unwrap();
    let _ = vm.run_exact(100).unwrap(); // Should be enough to confirm it never breaks out of the loop

    let result = vm.read_out();
    let lines = result.split_ascii_whitespace();
    for line in lines {
        assert_eq!(line, "0".to_string());
    }
}

#[test]
fn for_i_lt_10() {
    let mut vm = Vm::test();
    let source_code = "for (var i = 0; i < 10; i = i + 1) { print i; }";
    vm.compile(&source_code).unwrap();
    let _ = vm.run_exact(100).unwrap(); // Should be enough to confirm it never breaks out of the loop

    let result = vm.read_out();
    let lines = result.split_ascii_whitespace();
    for (index, line) in lines.clone().take(10).enumerate() {
        assert_eq!(line, format!("{index}"));
    }

    assert_eq!(None, lines.skip(10).next());
}
