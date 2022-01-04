;;;;;
print "\nTesting literals\n";

print "Testing...\n";
print true; print "\n";
print 42; print "\n";


print "\nTesting variable declarations\n";

var testStr = "testStr";
var testInt = -1;
var testBool = true;

print testStr; print "\n";
print testInt; print "\n";
print testBool; print "\n";


print "\nTesting variable assigning\n";
testStr = "assignedStr";
testInt = 4;
testBool = false;

print testStr; print "\n";
print testInt; print "\n";
print testBool; print "\n";


{
  print "\nTesting block\n";
}


if(true) {
  print "Testing if true\n";
}

if(false) {
  print "Testing if false\n";
}
else {
  print "Testing else false\n";
}


var x = true;
while (x) {
  print "\nTesting while\n";
  x = false;
}

while (testInt > 1) {
  print testInt;
  testInt = testInt - 1;
}

print "\nTesting math\n";

var add = (testInt + 1) * (4 * 2 + 1);
print add; print "\n";


print "\nTesting comparisons\n";

print (!(add < 50) && !(10 <= 2) || !true) == 2 < 1; print "\n";
;;;;;
