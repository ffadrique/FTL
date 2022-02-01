using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;

using System.IO;
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
        public Executer(String [] environment, String fname, out String stdout, out String stderr)
        {
            // Configure the execution options
            ProcessStartInfo psi = new ProcessStartInfo("cmd.exe")
            {
                CreateNoWindow = false,
                WindowStyle = ProcessWindowStyle.Hidden,
                UseShellExecute = false,
                RedirectStandardError = true,
                RedirectStandardOutput = true,
                RedirectStandardInput = true
            };

            // Spawn the process
            try
            {
                // Initialise the process
                using (Process proc = new Process())
                {
                    // Set process information
                    proc.StartInfo = psi;

                    // Initiate asynchronous standard output read
                    Sbout = new StringBuilder("");
                    proc.OutputDataReceived += StdoutHandler;

                    // Launch the process
                    proc.Start();

                    using (StreamWriter sw = proc.StandardInput)
                    {
                        if (sw.BaseStream.CanWrite)
                        {
                            foreach(String env in environment)
                                sw.WriteLine(env);
                            sw.WriteLine(fname);
                        }
                    }
                    
                    // Start reading standard outpurt and standard error
                    proc.BeginOutputReadLine();
                    stderr = proc.StandardError.ReadToEnd();

                    // Wait for process to terminate
                    proc.WaitForExit();

                    // Collect standard output and return
                    stdout = Sbout.ToString();
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
