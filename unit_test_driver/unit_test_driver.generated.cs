








using System;
using Microsoft.VisualStudio.TestTools.UnitTesting;

namespace UnitTestDriver
{

		[TestClass]
		public class frypto : Unit
		{

				[TestMethod]
				public void unit_m_aes()
				{
					// Initialise the test run
					Initialise("unit_m_aes");

					// Execute the fortran test if required
					if(OutOfDate) RunFortranTest();

					// Assert the test completion
					Assert.AreEqual(ProcessJxmlFile("m_aes"), 0);
				}

				[TestMethod]
				public void unit_m_base32()
				{
					// Initialise the test run
					Initialise("unit_m_base32");

					// Execute the fortran test if required
					if(OutOfDate) RunFortranTest();

					// Assert the test completion
					Assert.AreEqual(ProcessJxmlFile("m_base32"), 0);
				}

				[TestMethod]
				public void unit_m_base64()
				{
					// Initialise the test run
					Initialise("unit_m_base64");

					// Execute the fortran test if required
					if(OutOfDate) RunFortranTest();

					// Assert the test completion
					Assert.AreEqual(ProcessJxmlFile("m_base64"), 0);
				}

				[TestMethod]
				public void unit_m_blowfish()
				{
					// Initialise the test run
					Initialise("unit_m_blowfish");

					// Execute the fortran test if required
					if(OutOfDate) RunFortranTest();

					// Assert the test completion
					Assert.AreEqual(ProcessJxmlFile("m_blowfish"), 0);
				}

				[TestMethod]
				public void unit_m_crc32()
				{
					// Initialise the test run
					Initialise("unit_m_crc32");

					// Execute the fortran test if required
					if(OutOfDate) RunFortranTest();

					// Assert the test completion
					Assert.AreEqual(ProcessJxmlFile("m_crc32"), 0);
				}

				[TestMethod]
				public void unit_m_crypto()
				{
					// Initialise the test run
					Initialise("unit_m_crypto");

					// Execute the fortran test if required
					if(OutOfDate) RunFortranTest();

					// Assert the test completion
					Assert.AreEqual(ProcessJxmlFile("m_crypto"), 0);
				}

				[TestMethod]
				public void unit_m_des()
				{
					// Initialise the test run
					Initialise("unit_m_des");

					// Execute the fortran test if required
					if(OutOfDate) RunFortranTest();

					// Assert the test completion
					Assert.AreEqual(ProcessJxmlFile("m_des"), 0);
				}

				[TestMethod]
				public void unit_m_md5()
				{
					// Initialise the test run
					Initialise("unit_m_md5");

					// Execute the fortran test if required
					if(OutOfDate) RunFortranTest();

					// Assert the test completion
					Assert.AreEqual(ProcessJxmlFile("m_md5"), 0);
				}

				[TestMethod]
				public void unit_m_sha1()
				{
					// Initialise the test run
					Initialise("unit_m_sha1");

					// Execute the fortran test if required
					if(OutOfDate) RunFortranTest();

					// Assert the test completion
					Assert.AreEqual(ProcessJxmlFile("m_sha1"), 0);
				}

				[TestMethod]
				public void unit_m_sha256()
				{
					// Initialise the test run
					Initialise("unit_m_sha256");

					// Execute the fortran test if required
					if(OutOfDate) RunFortranTest();

					// Assert the test completion
					Assert.AreEqual(ProcessJxmlFile("m_sha256"), 0);
				}

				[TestMethod]
				public void unit_m_sha384()
				{
					// Initialise the test run
					Initialise("unit_m_sha384");

					// Execute the fortran test if required
					if(OutOfDate) RunFortranTest();

					// Assert the test completion
					Assert.AreEqual(ProcessJxmlFile("m_sha384"), 0);
				}

				[TestMethod]
				public void unit_m_sha512()
				{
					// Initialise the test run
					Initialise("unit_m_sha512");

					// Execute the fortran test if required
					if(OutOfDate) RunFortranTest();

					// Assert the test completion
					Assert.AreEqual(ProcessJxmlFile("m_sha512"), 0);
				}

				[TestMethod]
				public void unit_m_shasum()
				{
					// Initialise the test run
					Initialise("unit_m_shasum");

					// Execute the fortran test if required
					if(OutOfDate) RunFortranTest();

					// Assert the test completion
					Assert.AreEqual(ProcessJxmlFile("m_shasum"), 0);
				}

				[TestMethod]
				public void unit_m_uuencode()
				{
					// Initialise the test run
					Initialise("unit_m_uuencode");

					// Execute the fortran test if required
					if(OutOfDate) RunFortranTest();

					// Assert the test completion
					Assert.AreEqual(ProcessJxmlFile("m_uuencode"), 0);
				}

				[TestMethod]
				public void unit_m_uuid()
				{
					// Initialise the test run
					Initialise("unit_m_uuid");

					// Execute the fortran test if required
					if(OutOfDate) RunFortranTest();

					// Assert the test completion
					Assert.AreEqual(ProcessJxmlFile("m_uuid"), 0);
				}

		}

}
