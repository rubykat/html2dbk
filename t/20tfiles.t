# miscelaneous tests in separate files

#########################

use Test::More tests => 2;
use HTML::ToDocBook;

#########################

# compare two files
sub compare {
    my $file1 = shift;
    my $file2 = shift;

    if (!open(F1, $file1))
    {
    	print "error - $file1 did not open\n";
	return 0;
    }
    if (!open(F2, $file2))
    {
    	print "error - $file2 did not open\n";
	return 0;
    }

    my $res = 1;
    my $count = 0;
    while (<F1>)
    {
	$count++;
	my $comp1 = $_;
	# remove newline/carriage return (in case these aren't both Unix)
	$comp1 =~ s/\n//;
	$comp1 =~ s/\r//;

	my $comp2 = <F2>;

	# check if F2 has less lines than F1
	if (!defined $comp2)
	{
	    print "error - line $count does not exist in $file2\n  $file1 : $comp1\n";
	    close(F1);
	    close(F2);
	    return 0;
	}

	# remove newline/carriage return
	$comp2 =~ s/\n//;
	$comp2 =~ s/\r//;
	if ($comp1 ne $comp2)
	{
	    print "error - line $count not equal\n  $file1 : $comp1\n  $file2 : $comp2\n";
	    close(F1);
	    close(F2);
	    return 0;
	}
    }
    close(F1);

    # check if F2 has more lines than F1
    if (defined($comp2 = <F2>))
    {
	$comp2 =~ s/\n//;
	$comp2 =~ s/\r//;
	print "error - extra line in $file2 : '$comp2'\n";
	$res = 0;
    }

    close(F2);

    return $res;
}

#-----------------------------------------------------------------

my $conv = new HTML::ToDocBook();

my $bn = '';
my $test_count = 0;

$test_count++;
$bn = 'test1';
$result = $conv->convert(
infile=>"tfiles/${bn}.html"
);
ok($result, "($test_count) converted ${bn}.html");

# compare the files
$result = compare("tfiles/good_${bn}.xml", "tfiles/${bn}.xml");
ok($result, "($test_count) compare ${bn} matched");
if ($result) {
    unlink("tfiles/${bn}.xml");
}
