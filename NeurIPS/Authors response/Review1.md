#  Response to Review 1.

We thank the reviewer for the positive supports and helpful remarks. Below are our answers to your questions.

> First the nonlinear transforms do not preserve the low-rank structure of the original tensor, hence it is natural that the generated tensor can have higher rank. The data simply does not admit the CP model.

We respectfully point out this is the very argument that our work is motivated from. It is not surprising that classical CP method fails when data undergoes nonlinear transformations. What surprises is that a simple but careful ensemble learning method (via sign tensor series as we proposed) makes the estimation *provably* robust to unknown transformations, thereby effectively addressing the aforementioned challenges. Classical CPD achieves accuracy only when the data admits CP model; whereas our model achieves accuracy under *both* CP models and a wide range of non-CP models. Our method gains accuracy over low-rank models when data violates the low-rankness, whereas achieves equally optimal rates as low-rank CPD/Tucker when the data admits the low-rankness. 



> Second, even the transformed data can have higher rank than that of the original data, the numerical ranks at the relative approximation error of 0.1, shown in Figure 1, are not optimal, too high, perhaps due to incorrect decomposition. The authors do not explain the algorithm for CPD and its implementation used in their experiments.


We appreciate the working examples provided by the reviewer, and we are also able to reproduce the same calculation as the reviewer. 

With that said, our reported numbers in Figure 1(a) in Introduction section are also correct. We use default setting of built-in `cp` function (iteration = 25, tolerance = $10^{-5}$) in `rTensor` R package. The Figure 1(a) serves as a motivating example where the low-rank model fails to represent tensors obtained from nonlinear transformation. The numbers we reported are summary statistics averaged over random rank-3 tensors from Monte Carlo simulations. We agree with that, for some particular instances of random tensors and/or different precision configurations of R, the numerical rank increment may show up from 3 to $\approx$ 60, as calculated by reviewer. In fact, this result *does* support our conclusion in Introduction: 

1) Usual tensor rank is highly sensitive to nonlinear transformation, leading an rank increment from 3 (ground truth) to 60$\sim$150 (with numerical instability coming from randomness in signals, software configurations, optimization choices, etc); 

2) The rank increment is also highly sensitive to particular instances of signal tensors, despite of a common intrinsic rank $=3$ to start with. 

Both observations highlight the weakness of usual CP rank, suggesting the need for a new complexity measure invariant to unknown nonlinear transformation across all tensor instances. This has motivated us to develop robust high-rank tensor models. For easier reproducibility, we will add more instructions in our submitted code. We will also added more implementation details to the current section Appendix B.1 "Sensitivity of tensor rank to monotonic transformations". 


> This is to show that the authors do not achieve the best results for CPD. The results for two examples in Section 5 may face the same problem, and the conclusions are not very accurate or bias.


The CPD examples in Section 5 are calculated either from algebraic calculation (Table 1) or from cross validation (Figure 3). The rank in Table 1 is proved using algebraic equations (see Appendix Section B.2) for specially-structured tensors; therefore, no numerical precision concern is involved in the reported value. The prediction performance (Figure 3 and Table 2) automatically calibrates the rank selection because our comparison is based on cross validation error. We set the rank of CPD to be the one that yields the best prediction performance over the range $r\in \{1,2,\ldots\}$. Therefore, the conclusions and comparison are valid.  


For reviewer's interest, we have added extra numerical experiments and verified that different implementations of CPD barely affect the results in Section 5. Specifically, we generated signal tensors based on Models 2-3 in Table 1 (L325-326) and used new R settings (denoted "CPD2", iteration = $10^3$, tolerance = $10^{-5}$, so as to mimic reviewer's working examples). The following two tables summarize the errors of new CPD2 (in column 4). We compare the summary statistics in means and standard errors between CPD and CPD2. The results show that CPD2 yields similar performance as default CPD except reducing standard errors. Both implementations of CPD exhibit worse performance than our nonparametric models. This observation suggests that, compared to the algorithmic error, the statistical error is likely more dominating in practice. 


> The proposed method approximates the tensor by a sum of $H=20$ (or even more) low-rank tensors. Each estimated tensor can be low rank but not the mean tensor of low-rank tensors. For a fair comparison, the authors should do the CPD completion method similarly, the final result should be mean of low-rank tensor approximated by CPD.


We have run additional numerical experiments to implement the alternative CPD suggested by the reviewer (denoted CPD3 in column 5 of the above two tables). We took $H=20$ low-rank tensors from CPD and used the entrywise average as the estimate. The results confirm that entrywise average of low-rank tensors shows no improvements in performance by destroying the intrinsic low-rankness in the data. In contrast, our proposed sign series approach achieves accurate estimation under both linear and nonlinear models. In particular, sign-series averaged tensor maintains low-rankness if the data admits low-rankness, while gaining provable accuracy over CPD when data violates low-rankness. From the comparison, we show that our nonparametric sign tensor estimation outperforms a wide range of existing low-rank models. 



 *Model 2 at d = 20,40,60: $^*$We use the scale of $10^{-2}$ scale and drop to 0 when numbers below $10^{-4}$ for space consideration*

 Tensor dimension  | NonparaT (ours) | CPD (default) |  CPD2 (new) | CPD3 (new)$^*$  |Tucker$^*$
 ------------------|-----------------|---------------|-----------------|----------|------
 $d = 20$ | **5.05 (.16)** | 6.05 (.04) | 6.05 (.03) | 6.04 (0) |  6.06 (0)
 $d = 40$ | **4.74 (.13)** | 5.66 (.01) | 5.66 (.01) | 5.65 (0)|  5.67 (0)
 $d = 60$ | **4.68 (.11)** | 5.56 (.01) | 5.56 (0) | 5.56 (0) | 5.56 (0)


    
 *Model 3 at d = 20,40,60:*

 Tensor dimension | NonparaT (ours) | CPD (default) | CPD2 (new)|  CPD3 (new)$^*$ | Tucker$^*$
 ------------------|-----------------|---------------|-----------------|----------|----------
 $d = 20$ | **1.90 (.08)** | 2.68 (.04) |2.68 (.02) | 2.68 (0) | 2.67 (0)
 $d = 40$ |  **1.33 (.05)** | 2.53 (.03) |  2.53 (.01) | 2.53 (0) | 2.52 (0)
 $d = 60$ | **1.13 (.03)** | 2.51 (.01) | 2.49 (.01) |  2.49 (0) | 2.48 (0)
