import subprocess


def run(filepath, expectedOutput):
    proc = subprocess.Popen(["./w", filepath], stdout=subprocess.PIPE)
    out = proc.communicate()[0].decode("utf-8")
    
    print("Expected Output:", expectedOutput)
    print("Actual Output:  ", out)
    
    if out == expectedOutput:
        print("Test passed\n")
    else:
        print("Test failed\n")


if __name__ == "__main__":
    run("factorial.w", "result is 2432902008176640000\n")
    run("fibonacci.w", "0\n1\n1\n2\n3\n5\n8\n13\n21\n34\n55\n89\n144\n233\n377\n610\n987\n1597\n2584\n4181\n")
    run("isPrime.w", "1215121 is prime :)\n")
