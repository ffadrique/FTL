using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;

using System.Xml;
using System.IO;

namespace UnitTestDriver
{
    /// <summary>
    /// Class to manage the Visual Studio user configuration
    /// </summary>
    class UserConfiguration
    {
        /// <summary>
        /// File search string pattern
        /// </summary>
        const String Pattern = ".vfproj.*.user";

        /// <summary>
        /// Environment attribute value
        /// </summary>
        const String EnvAttribute = "XFUNIT_ROOT_DIR=$(ProjectDir)..";

        /// <summary>
        /// Constructor
        /// </summary>
        /// <param name="path">Project path</param>
        /// <param name="path">Test name</param>
        public UserConfiguration(String path, String name)
        {
            // List of already existing user files
            List<String> ufiles = GetUserFiles(path, name);

            // Loop on the files
            foreach (String ufile in ufiles)
            {
                // Process the file
                ProcessUserFile(ufile);
            }

            // Check if the current user file exists
            String user = Environment.UserName;
            String thisUserFile = Path.Combine(path, name + Pattern.Replace("*", user));
            if (!File.Exists(thisUserFile))
            {
                // Create the file for this user
                CreateUserFile(thisUserFile);
            }

        }

        /// <summary>
        /// Retrieve the list of visual studio user files
        /// </summary>
        /// <param name="path">Project path</param>
        /// <param name="name">Test name</param>
        /// <returns>List of visual studio user files</returns>
        private List<String> GetUserFiles(String path, String name)
        {
            // Existing files
            List<String> files = Directory.GetFiles(path, name + Pattern).ToList();

            // Retunr the list of files
            return files;
        }

        /// <summary>
        /// Process a visual studio user file
        /// The file is updated in place
        /// </summary>
        /// <param name="ufile">Visual studio user file</param>
        private void ProcessUserFile(String ufile)
        {
            // Load the XML file
            XmlDocument doc = new XmlDocument();
            doc.Load(ufile);

            // Select the nodes that contain environment information
            XmlNodeList nodes = doc.SelectNodes("/VisualStudioUserFile/Configurations/Configuration");

            // Loop on the nodes
            foreach (XmlNode node in nodes)
            {
                // Check if the node contains the environment attribute
                if (node.Attributes["Environment"] == null)
                {
                    // Add the environment attribute
                    XmlAttribute attr = doc.CreateAttribute("Environment");
                    attr.Value = EnvAttribute;
                    node.Attributes.Append(attr);
                }
            }

            // Re-write the XML file
            WriteUserFile(ufile, doc);
        }

        /// <summary>
        /// Create a visual studio user file
        /// </summary>
        /// <param name="ufile">Visual studio user file</param>
        private void CreateUserFile(String ufile)
        {
            // Initialise document
            XmlDocument doc = new XmlDocument();

            // Create the root element
            XmlElement root = doc.CreateElement("VisualStudioUserFile");
            XmlAttribute attr = doc.CreateAttribute("ProjectCreator");
            attr.Value = "Intel Fortran";
            root.Attributes.Append(attr);
            attr = doc.CreateAttribute("Keyword");
            attr.Value = "Console Application";
            root.Attributes.Append(attr);
            attr = doc.CreateAttribute("Version");
            attr.Value = "11.0";
            root.Attributes.Append(attr);

            // Create the list of configurations
            XmlElement configurations = doc.CreateElement("Configurations");

            // Initialise element and attribute common items
            XmlElement e = doc.CreateElement("Configuration");
            XmlAttribute name = doc.CreateAttribute("Name");
            XmlAttribute env = doc.CreateAttribute("Environment");
            env.Value = EnvAttribute;

            // Loop on the configurations to generate
            foreach (String platform in new String[] { "Win32", "x64" })
                foreach (String config in new String[] { "Debug", "Release" })
                {
                    // Define the name value
                    name.Value = config + "|" + platform;

                    // Add attributes
                    e.Attributes.Append(name);
                    e.Attributes.Append(env);

                    // Add the configuration element to the list of configurations
                    configurations.AppendChild(e.Clone());
                }

            // Add the list of configurations to the root elelemt
            root.AppendChild(configurations);

            // Add the root elelment to the document
            doc.AppendChild(root);

            // Re-write the XML file
            WriteUserFile(ufile, doc);
        }

        /// <summary>
        /// Write XML visual studio user file
        /// </summary>
        /// <param name="ufile">Path of the target user file</param>
        /// <param name="xdoc">XML document to write</param>
        private void WriteUserFile(String ufile, XmlDocument xdoc)
        {
            // Define the writer settings
            XmlWriterSettings settings = new XmlWriterSettings
            {
                Indent = true,
                IndentChars = "  ",
                NewLineChars = Environment.NewLine
            };

            // Create the XML writer
            using (XmlWriter writer = XmlWriter.Create(ufile, settings))
            {
                // Write file to writer
                xdoc.WriteTo(writer);
            }

        }
    }
}
