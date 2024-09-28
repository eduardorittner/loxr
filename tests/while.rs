use loxr::Value;
use loxr::Vm;

#[test]
fn endless_while() {
    let mut vm = Vm::test();
    let source_code = "while(true) print 1;";
    vm.compile(&source_code).unwrap();
    let _ = vm.run_exact(100).unwrap(); // Should be enough to confirm it never breaks out of the loop
    let result = vm.read_out();
    let lines = result.split_ascii_whitespace();

    for line in lines {
        assert_eq!(line, "1")
    }
}

#[test]
fn while_10_iters() {
    let mut vm = Vm::test();
    let source_code = "var i = 0; while(i < 10) { print i; i = i + 1; }";
    vm.compile(&source_code).unwrap();
    let _ = vm.run().unwrap();

    let result = vm.read_out();
    let lines = result.split_ascii_whitespace();

    for (index, line) in lines.enumerate() {
        assert_eq!(line, format!("{index}"))
    }
}

#[test]
fn while_zero_iter() {
    let mut vm = Vm::test();
    let source_code = "while(false) print 0;";
    vm.compile(&source_code).unwrap();
    let _ = vm.run().unwrap();

    let result = vm.read_out();
    assert_eq!(result, "".to_string());
}
