#include <stdio.h>

int x = 42;
int y = -1;

enum WeekDay
{
    Monday = 1,
    Tuesday = 2,
    Wednesday = 3,
    Thursday = 4,
    Friday = 5,
    Saturday = 6,
    Sunday = 7
};

const char* get_quantifier(unsigned int num)
{
    static const char* quantifiers[] = {"st", "nd", "rd", "th"};
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

int main()
{
    enum WeekDay sunday = Sunday;
    printf("Sunday is the %i%s day of the week\n", (int)sunday, get_quantifier(sunday));
}
