import subprocess

EXPECTED_OUTPUT_INDEX = 1
TEST_NAME_INDEX = 0



##########################################################################
######################### Define expected output #########################
##########################################################################
test_print_str_expected_output = [
    "one",
    "two three",
]

test_print_num_expected_output = [
    "123",
    "987654",
    "0",
    "-0",
    "123.456",
    "-1.0e-3",
]

test_unary_expected_output = [
    "false",
    "false",
    "true",
    "1",
]

test_comparison_expected_output = [
    "false",
    "false",
    "true",
    "true",
    "true",
    "false",
    "false",
    "true",
    "true",
    #
    "true",
    "true",
    "false",
    "false",
    "c",
    #
    "1",
    "1",
    "true",
    "false",
    "false",
    "false",
    "true",
]

test_scope_expected_output = [
    "inner inner",
    "inner inner",
]

test_while_expected_output = [
    "1",
    "2",
    "3",
]

test_if_expected_output = [
    "In if-block",
    "10",
    "9",
    "good",
    "good",
    "block",
]

test_dangling_expected_output = [
    "good",
]

test_var_expected_output = [
    "nil",
    "5",
    "2",
    "2",
    "9",
    "8",
]

test_misc_expr_expected_output = [
    "4",
    "4",
    "4",
    "4.5",
]

test_associativity_expected_output = [
    "c",
    "c",
    "c",
    "var",
    "var",
]

test_reassign_expected_output = [
    "before",
    "after",
    "arg",
    "arg",
]

test_local_expected_output = [
    "before",
    "after",
    "arg",
    "arg",
]

test_equality_expected_output = [
    "true",
    "false",
    "false",
    "true",
    "false",
    "false",
    "false",
    "false",
    "false",
    "false",
    "true",
    "true",
    "false",
    "true",
    "true",
    "true",
    "true",
    "true",
]

test_truthy_expected_output = [
    "false",
    "nil",
    "true",
    "0",
    "empty",
]

test_precedence_expected_output = [
    "14",
    "8",
    "4",
    "0",
    "true",
    "true",
    "true",
    "true",
    "0",
    "0",
    "0",
    "0",
    "4",
]

test_redeclare_expected_output = [
    "lox.hs: Error: Redeclaring variable 'in_if'",
]


tests = [
    # --------------- Correct input tests ---------------------------
    ("TestPrintStr.lox", test_print_str_expected_output),
    ("TestPrintNum.lox", test_print_num_expected_output),
    ("TestUnary.lox", test_unary_expected_output),
    ("TestComparison.lox", test_comparison_expected_output),
    ("TestScope.lox", test_scope_expected_output),
    ("TestWhile.lox", test_while_expected_output),
    ("TestIf.lox", test_if_expected_output),
    ("TestDangling.lox", test_dangling_expected_output),
    ("TestVar.lox", test_var_expected_output),
    ("TestMiscExpr.lox", test_misc_expr_expected_output),
    ("TestAssociativity.lox", test_associativity_expected_output),
    ("TestReassign.lox", test_reassign_expected_output),
    ("TestLocal.lox", test_local_expected_output),
    ("TestEquality.lox", test_equality_expected_output),
    ("TestPrecedence.lox", test_precedence_expected_output),
    ("TestTruthy.lox", test_truthy_expected_output),
    # -------------------- Error tests ------------------------------
    ("TestRedeclare.lox", test_redeclare_expected_output),
    ]






##########################################################################
########################### Test functions ###############################
##########################################################################
def run_test(test_name):
    """ run_test - Runs a given test in a subprocess and
                   captures the output.
        
        @param test_name The name of test test file.
    """
    # Run test and capture output
    result = subprocess.run(["runhaskell", "./lox.hs", "./tests/interpreter/" + str(test_name)], capture_output=True, text=True)
    output = result.stdout
    error = result.stderr
    
    #print(output)
    if error != "":
        #print(str(error), end='')
        output = str(error).replace("\n", "", 1)

    return output


def print_red(input_string):
    """ print_red - Print a given string to the terminal
        in red without a new line.
        
        @param input_string The string to print in red.
    """
    print("\033[91m {}\033[00m".format(input_string), end = '')
 
 
def print_green(input_string):
    """ print_green - Print a given string to the terminal
        in green without a new line.

        @param input_string The string to print in green.
    """
    print("\033[92m {}\033[00m" .format(input_string), end = '')


if __name__ == "__main__":
    for test in tests:
        num_errors = 0
        num_passing = 0
        i = 0

        output_lines = run_test(test[TEST_NAME_INDEX]).splitlines()

        for expected_line in test[EXPECTED_OUTPUT_INDEX]:
            if output_lines[i] != expected_line:
                print_red("ERROR")
                print("  Got: " + str(output_lines[i]) + "\n\tExpected: " + str(expected_line))
                num_errors = num_errors + 1
            else:
                num_passing = num_passing + 1
            i = i + 1
        
        if num_errors == 0:
            print_green("PASSED ")
            print(test[TEST_NAME_INDEX])
        else:
            print_red("FAILD ")
            print(test[TEST_NAME_INDEX])