/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */

package test.math

object BooleanCombinations {
    private val values = {
        (false, false, false, false) ::
        (false, false, false, true ) ::
        (false, false, true,  false) ::
        (false, false, true,  true ) ::
        (false, true,  false, false) ::
        (false, true,  false, true ) ::
        (false, true,  true,  false) ::
        (false, true,  true,  true ) ::
        (true,  false, false, false) ::
        (true,  false, false, true ) ::
        (true,  false, true,  false) ::
        (true,  false, true,  true ) ::
        (true,  true,  false, false) ::
        (true,  true,  false, true ) ::
        (true,  true,  true,  false) ::
        (true,  true,  true,  true ) ::
        Nil
    }

    def test(testCase: (Boolean, Boolean, Boolean, Boolean) => Unit) {
        for ((x, y, z, w) <- values) {
            testCase(x, y, z, w)
        }
    }
}
