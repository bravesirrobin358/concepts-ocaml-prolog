
using System;
using System.Collections;

namespace Namespace
{
    class Shape {

    }
    class Triangle : Shape {

    }
    class Program {
        static void Main(String[] args){
            List<int> x = new List<int>();

            x.Add(3);
            if (x is List<int>){
                Console.WriteLine(x.GetType().GetGenericArguments()[0]);
            }

        }
    }
}