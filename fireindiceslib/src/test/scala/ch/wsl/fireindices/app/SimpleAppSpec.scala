/*
 * To change this license header, choose License Headers in Project Properties.
 * To change this template file, choose Tools | Templates
 * and open the template in the editor.
 */

package ch.wsl.fireindices.app

import org.scalatest.Suites

object Dummy

class SimpleAppSpec extends Suites(
    new CalculateSpec(Dummy),
    new CompleteSpec(Dummy),
    new CompleteContinuousSpec(Dummy),
    new ReplaceSpec(Dummy),
    new ResultsSpec(Dummy)
  
)
