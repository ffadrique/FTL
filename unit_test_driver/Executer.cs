using System;
using System.Collections.Generic;
using System.Text;
using System.Diagnostics;

namespace UnitTestDriver
{
    class Executer
    {
        /// <summary>
        /// Generic string builder for process exectuion buffering
        /// </summary>
        private static StringBuilder Sbout { get; set; }

        /// <summary>
        /// Execute an external application/process
        /// </summary>
        /// <param name="environment">Environment variable</param>
        /// <param name="fname">Name of the file to execute</param>
        /// <param name="stdout">Returned standard output</param>
        /// <param name="stderr">Returned standard error</param>
        public Executer(Dictionary<String,String> environment, String fname, out String stdout, out String stderr)
        {
            // Set the environment
            foreach (String env in environment.Keys)
                Environment.SetEnvironmentVariable(env, environment[env]);

            // Spawn the process
            try
            {
                // Initialise the process
                using (Process proc = new Process())
                {
                    // Set process information
                    proc.StartInfo.FileName = fname;
                    proc.StartInfo.CreateNoWindow = false;
                    proc.StartInfo.WindowStyle = ProcessWindowStyle.Hidden;
                    proc.StartInfo.UseShellExecute = false;
                    proc.StartInfo.RedirectStandardError = true;
                    proc.StartInfo.RedirectStandardOutput = true;
                    proc.StartInfo.RedirectStandardInput = true;

                    // Launch the process
                    proc.Start();

                    // Start reading standard outpurt and standard error
                    stdout = proc.StandardOutput.ReadToEnd();
                    stderr = proc.StandardError.ReadToEnd();

                    // Wait for process to terminate
                    proc.WaitForExit();
                }
            }
            catch (Exception ex)
            {
                throw new Exception("Executing '" + fname + "'", ex);
            }
        }

        /// <summary>
        /// Handler for standard output asynchronous read
        /// </summary>
        /// <param name="sender">Process sending the signal</param>
        /// <param name="outLine">Event arguments</param>
        private static void StdoutHandler(object sender, DataReceivedEventArgs e)
        {
            // Collect the command output
            if (!String.IsNullOrEmpty(e.Data))
            {
                Sbout.Append(e.Data);
            }
        }
    }
}
