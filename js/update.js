//  update - from http://rainbow.arch.scriptmania.com/scripts
function getLongDateString()
{	//method defined on class Date.
	//Returns a date string of the form: Day DD Month,YYYY
	//(e.g. Sunday 27 September, 1998)
	monthNames = new Array("January","February","March","April","May","June","July","August","September","October","November","December");
dayNames = new Array("Sunday","Monday","Tuesday","Wednesday","Thursday","Friday","Saturday");
	dateOfMonth = this.getDate();
monthNo = this.getMonth();
	month = monthNames[monthNo];
year = this.getYear();
	if (year < 2000)
year = year + 1900;
dateStr = dateOfMonth+" "+month+", "+year;
	return dateStr;
}
//register the  method in the class Date
Date.prototype.getLongDateString=getLongDateString;

function DocDate()
{ //return the document modification date (excl.time)
//as a string
	DateTimeStr = document.lastModified;
	secOffset = Date.parse(DateTimeStr);
	if (secOffset == 0 || secOffset == null) //Opera3.2
			 dateStr = "Unknown";
	else
	{
		aDate = new Date();
		aDate.setTime(secOffset);
		//use method defined above
		datestr = aDate.getLongDateString();
	}
	return dateStr;
}

document.write(" Last updated: ");
document.writeln(DocDate() );
//
