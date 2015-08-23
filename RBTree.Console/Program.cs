using System;
using System.Collections.Generic;
using System.Diagnostics;
using System.Linq;
using Akka.Util.Internal.Collections;
using RBTree;


namespace RBTree.Console
{
    class Program
    {
        static void Main(string[] args)
        {
            const int tests = 1000000;
            var tree = RBTree<int>.Empty;
			var tree2 = RBTree2.RBTree<int>.Empty;
            var set = ImmutableTreeSet<int>.Empty;
            var random = new Random();
            var randoms = Enumerable.Range(0, tests).Select(_ => random.Next()).ToList();

            // INSERT
			Execute("rb  insert", () => { randoms.ForEach(r => tree = tree.Add(r)); return tree; });
			Execute("rb2  insert", () => { randoms.ForEach(r => tree2 = tree2.Add(r)); return tree2; });
            Execute("set insert", () => { randoms.ForEach(r => set = set.Add(r)); return set; });
            //var fset = new Microsoft.FSharp.Collections.FSharpSet<int>(Enumerable.Empty<int>());
            //Execute("fset insert", () => { randoms.ForEach(r => fset = fset.Add(r)); return fset; });
            //var immSet = System.Collections.Immutable.ImmutableHashSet<int>.Empty;
            //Execute("immset insert", () => { randoms.ForEach(r => immSet = immSet.Add(r)); return set; });

            System.Console.WriteLine();
            //randoms = Enumerable.Range(0, tests).Select(_ => random.Next()).ToList();
            // LOOKUP
			Execute("rb  contains", () => { randoms.ForEach(r => tree.Contains(r)); return tree; });
			Execute("rb2  contains", () => { randoms.ForEach(r => tree2.Contains(r)); return tree2; });
            Execute("set  contains", () => { randoms.ForEach(r => set.Contains(r)); return tree; });

			System.Console.WriteLine();
            // DELETE
			Execute("rb  remove", () => { randoms.ForEach(r => tree = tree.Remove(r)); return tree; });
			Execute("rb2  remove", () => { randoms.ForEach(r => tree2 = tree2.Remove(r)); return tree2; });
            Execute("set  remove", () => { randoms.ForEach(r => set = set.Remove(r)); return tree; });
            //Execute("rb  remove", () => { randoms.ForEach(r => fset = fset.Remove(r)); return tree; });
            //Execute("rb  remove", () => { randoms.ForEach(r => immSet = immSet.Remove(r)); return tree; });
            
        }

        private static T Execute<T>(string name, Func<T> action)
        {
            var sw = new Stopwatch();
            sw.Start();
            var result = action();
            sw.Stop();

            System.Console.WriteLine("{0}: {1}", name, sw.ElapsedMilliseconds);
            return result;
        }
    }
}
