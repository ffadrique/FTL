using System;
using System.Collections.Generic;
using System.Linq;

using System.IO;
using System.Xml;
using System.Diagnostics;

using System.Runtime.InteropServices;

namespace UnitTestDriver
{
    public class Unit
    {
        /// <summary>
        /// Test name
        /// </summary>
        private String TestName = String.Empty;

        /// <summary>
        /// Test execution directory
        /// </summary>
        private String TestDir = String.Empty;

        /// <summary>
        /// Full path to the test executable
        /// </summary>
        private String Binary = String.Empty;

        /// <summary>
        /// Full path to the output .jxml file
        /// </summary>
        private String Jxml = String.Empty;

        /// <summary>
        /// Flag that identifies the test out of date (binary newer than .jxml)
        /// </summary>
        public Boolean OutOfDate { get; set; }

        /// <summary>
        /// Default constructor
        /// </summary>
        public Unit()
        {
        }

        /// <summary>
        /// Initialises fortran unit test
        /// </summary>
        /// <param name="test_name">Name of the test to run</param>
        public void Initialise(String test_name)
        {
            // Initialise the test name
            TestName = test_name;

            // Get the solution path
            String solution_path = GetSolutionPath();

            // Get the test executable
            Binary = GetBinary(solution_path, test_name);

            // Look for the .jxml file
            String[] jxmls = Directory.GetFiles(solution_path, test_name.Replace("unit_", "") + ".jxml", SearchOption.AllDirectories);
            if (jxmls != null && jxmls.Count() > 0)
                Jxml = jxmls[0];
            else
                Jxml = String.Empty;

            // Get the testing directory
            Int32 idx = Binary.IndexOf(test_name);
            TestDir = Binary.Substring(0, idx);

            // Check Jxml file
            if (Jxml == String.Empty)
            {
                // Test must run
                OutOfDate = true;
            }
            else
            {
                // Get the timestamps of the Binary and the Jxml files
                DateTime BinaryTstamp = File.GetLastWriteTime(Binary);
                DateTime JxmlTstamp = File.GetLastWriteTime(Jxml);

                // Test must run if binary is newer than jxml file
                OutOfDate = (BinaryTstamp > JxmlTstamp);
            }

            // Review the visual studio user configuration files
            new UserConfiguration(Path.Combine(TestDir, TestName), TestName);
        }

        /// <summary>
        /// Get the binary executable file for a given test
        /// </summary>
        /// <param name="solutionPath">Path for the .sln file</param>
        /// <param name="testName">Name of the test to execute</param>
        /// <returns></returns>
        private String GetBinary(String solutionPath, String testName)
        {
            // Get the active configuration
            EnvDTE.DTE dte = (EnvDTE.DTE)Marshal.GetActiveObject("VisualStudio.DTE");
            EnvDTE.SolutionBuild sb = dte.Solution.SolutionBuild;
            EnvDTE.SolutionConfiguration sc = sb.ActiveConfiguration;
            String activeConfiguration = sc.Name;

            // Get the binaries candidates for the selected test
            String[] binaries = Directory.GetFiles(solutionPath, testName + ".exe", SearchOption.AllDirectories);

            // Return the binary for the active configuration
            return (binaries.Where(p => p.Contains(activeConfiguration)) ?? new String[] { }).ToArray()[0];
        }

        /// <summary>
        /// Run a fortran test
        /// </summary>
        public void RunFortranTest()
        {
            // Initialise the test command environment
            Dictionary<String,String> env = new Dictionary<String,String>();
            env.Add("XFUNIT_ROOT_DIR", TestDir);
            env.Add("PROF_DIR", Path.GetDirectoryName(TestDir));
            
            // Initialise the test command
            String cmd = @"""" + Binary + @"""";
            cmd = Binary;

            // Get the current path
            String CurDir = Directory.GetCurrentDirectory();

            // Execute the test
            try
            {
                // Set the running path
                Directory.SetCurrentDirectory(Path.Combine(TestDir, TestName));

                // Execute the fortran binary
                // *** Compile the binary with /libs:static or
                // *** set windows path to folder where libcoremd.dll is located
                new Executer(env, cmd, out String stdout, out String stderr);
                if (stderr != String.Empty)
                {
                    throw new Exception("Errors during fortran text execution" + Environment.NewLine + stderr);
                }
            }
            catch (Exception ex)
            {
                throw new Exception("Error running fortran test", ex);
            }
            finally
            {
                // Restore the current directory
                Directory.SetCurrentDirectory(CurDir);
            }
        }

        /// <summary>
        /// Process the .jxml file of a fortran test
        /// </summary>
        /// <param name="test_name">Name of the test for the .jxml file</param>
        public Int32 ProcessJxmlFile(String test_name)
        {
            // Look for the .jxml file (if it did not exist at initialisation Junit has no info)
            if (Jxml == String.Empty)
            {
                String solution_path = GetSolutionPath();
                String[] jxmls = Directory.GetFiles(solution_path, test_name + ".jxml", SearchOption.AllDirectories);
                Jxml = jxmls[0];
            }

            // Load the .jxml file
            XmlDocument jxmldoc = new XmlDocument();
            jxmldoc.Load(Jxml);

            // Get the .jxml failures
            XmlNode fail_node = jxmldoc.DocumentElement.SelectSingleNode("/testsuite/@failures");

            // Get the .jxml errors
            XmlNode error_node = jxmldoc.DocumentElement.SelectSingleNode("/testsuite/@errors");

            // Return the total number of failures and errors
            return Convert.ToInt32(fail_node.Value + error_node.Value);
        }

        /// <summary>
        /// Get the test name from the test calling method (2 levels above)
        /// </summary>
        /// <results>The test name</results>
        public static String GetTestName()
        {
            StackTrace st = new StackTrace();
            StackFrame sf = st.GetFrame(1);
            return sf.GetMethod().Name;
        }

        /// <summary>
        /// Get the solution path
        /// </summary>
        /// <results>The solution path</results>
        public static String GetSolutionPath()
        {
            EnvDTE.DTE dte = (EnvDTE.DTE)System.Runtime.InteropServices.Marshal.GetActiveObject("VisualStudio.DTE");
            return Path.GetDirectoryName(dte.Solution.FullName);
        }
    }
}
