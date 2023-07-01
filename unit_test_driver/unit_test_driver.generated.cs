
using System;
using Microsoft.VisualStudio.TestTools.UnitTesting;

namespace UnitTestDriver
{

		[TestClass]
		public class ftl : Unit
		{
				[TestMethod]
				public void unit_list()
				{
					// Initialise the test run
					Initialise("unit_list");

					// Execute the fortran test if required
					if(OutOfDate) RunFortranTest();

					// Assert the test completion
					Assert.AreEqual(ProcessJxmlFile( "xxbase___list_ftl" ), 0);
				}
				[TestMethod]
				public void unit_queue()
				{
					// Initialise the test run
					Initialise("unit_queue");

					// Execute the fortran test if required
					if(OutOfDate) RunFortranTest();

					// Assert the test completion
					Assert.AreEqual(ProcessJxmlFile( "xxbase___queue_ftl" ), 0);
				}
				[TestMethod]
				public void unit_slist()
				{
					// Initialise the test run
					Initialise("unit_slist");

					// Execute the fortran test if required
					if(OutOfDate) RunFortranTest();

					// Assert the test completion
					Assert.AreEqual(ProcessJxmlFile( "xxbase___slist_ftl" ), 0);
				}
				[TestMethod]
				public void unit_stack()
				{
					// Initialise the test run
					Initialise("unit_stack");

					// Execute the fortran test if required
					if(OutOfDate) RunFortranTest();

					// Assert the test completion
					Assert.AreEqual(ProcessJxmlFile( "xxbase___stack_ftl" ), 0);
				}
				[TestMethod]
				public void unit_tree()
				{
					// Initialise the test run
					Initialise("unit_tree");

					// Execute the fortran test if required
					if(OutOfDate) RunFortranTest();

					// Assert the test completion
					Assert.AreEqual(ProcessJxmlFile( "xxbase___tree_ftl" ), 0);
				}
				[TestMethod]
				public void unit_vector()
				{
					// Initialise the test run
					Initialise("unit_vector");

					// Execute the fortran test if required
					if(OutOfDate) RunFortranTest();

					// Assert the test completion
					Assert.AreEqual(ProcessJxmlFile( "xxbase___vector_ftl" ), 0);
				}
		}
}
