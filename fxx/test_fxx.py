import os
import io

import unittest

class Test_fxx(unittest.TestCase):

    def test_generate_basic(self):

        input = "utest/m_xfunit_suite.t90"
        reference = "utest/reference/m_xfunit_suite.f90"
        output = "utest/m_xfunit_suite.f90"
        template_reference = "utest/reference/m_xfunit_unit_list_ftl.f90"
        template_output = "utest/m_xfunit_unit_list_ftl.f90"

        os.system("py fxx.py " + input)

        lreference = list(io.open(reference))
        loutput = list(io.open(output))
        lreference.pop() # Pop to remove the timestamp
        loutput.pop()
        self.assertListEqual(loutput, lreference)

        lreference = list(io.open(template_reference))
        loutput = list(io.open(template_output))
        lreference.pop()
        loutput.pop()
        self.assertListEqual(loutput, lreference)


    def test_generate_multiple(self):

        input = "utest/m_eop_list.t90"
        reference = "utest/reference/m_eop_list.f90"
        output = "utest/m_eop_list.f90"
        template_reference_1 = "utest/reference/m_eop_vector_ftl.f90"
        template_output_1 = "utest/m_eop_vector_ftl.f90"
        template_reference_2 = "utest/reference/m_leap_list_ftl.f90"
        template_output_2 = "utest/m_leap_list_ftl.f90"

        os.system("py fxx.py " + input)

        lreference = list(io.open(reference))
        loutput = list(io.open(output))
        lreference.pop()
        loutput.pop()
        self.assertListEqual(loutput, lreference)

        lreference = list(io.open(template_reference_1))
        loutput = list(io.open(template_output_1))
        lreference.pop()
        loutput.pop()
        self.assertListEqual(loutput, lreference)

        lreference = list(io.open(template_reference_2))
        loutput = list(io.open(template_output_2))
        lreference.pop()
        loutput.pop()
        self.assertListEqual(loutput, lreference)


if __name__ == '__main__':
    unittest.main()
