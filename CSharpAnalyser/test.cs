using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;
using System.Text.RegularExpressions;

namespace ConsoleApplication1
{
    class Program
    {
        static int xxx(int x, int y)
        {
            return x + y;
        }

        static void Main(string[] args)
        {
            Regex.Match("my text", @"\pXXX");
            int y, xd;
            var x = 5 + 4;
            x = 11;
            if (x == 11)
            {
                var c = 17 + 5;
                if (c < 17)
                {
                    x = 11;
                }
                else
                {
                    x = 17;
                }
                Console.WriteLine(c + 1);
            }
            else if (x == 6)
            {
                x = x + 5;
            }
            Console.WriteLine(x);
        }
    }
}
