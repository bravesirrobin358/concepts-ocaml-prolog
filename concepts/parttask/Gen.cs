using System;
using System.Collections;

class Gen
{
    static void Main()
    {
        // Create an ArrayList
        List<int> myArrayList = new ArrayList();

        // Add some elements to the ArrayList
        myArrayList.Add(1);

        // Print the class type of the ArrayList
        Console.WriteLine("The class of myArrayList is: " + myArrayList.GetType());
    }
}
