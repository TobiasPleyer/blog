#include <stdio.h>

int x = 42;
int y = -1;

enum WeekDay
{
    Mon = 1,
    Tue = 2,
    Wed = 3,
    Thu = 4,
    Fri = 5,
    Sat = 6,
    Sun = 7
};

const char* quantifiers[] = {"st", "nd", "rd", "th"};

const char* get_quantifier(unsigned int num)
{
    switch (num)
    {
        case 1:
            return quantifiers[0];
        case 2:
            return quantifiers[1];
        case 3:
            return quantifiers[2];
        default:
            return quantifiers[3];
    }
}

enum WeekDay mon = Mon;
enum WeekDay tue = Tue;
enum WeekDay wed = Wed;
enum WeekDay thu = Thu;

int main()
{
    printf("Monday is the %i%s day of the week\n", (int)mon, get_quantifier(mon));
    printf("Tuesday is the %i%s day of the week\n", (int)tue, get_quantifier(tue));
    printf("Wednesday is the %i%s day of the week\n", (int)wed, get_quantifier(wed));
    printf("Thursday is the %i%s day of the week\n", (int)thu, get_quantifier(thu));
}
