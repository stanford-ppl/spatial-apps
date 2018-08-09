import virtualized._
import spatial.dsl._



// /*
//   Sketch of app:


//     ################
//     # Inner Kernel #
//     ################
//                        diag_index                              
//                      __↓_______        ___________________
//                     |\         |      |                   |
//             diag_index\        |  diag_index      B       |
//                    ↳|  o   L   |     ↳|===================|
//                  ↱  |   \      |      |                   |
//                  |  |    \     |      |                   |
//                  |  |     \    |      |                   |
//              len {  |      \   |      |                   |
//                  |  |       \  |      |                   |
//                  |  |        \ |      |                   |
//                  ↳  |_________\|      |___________________|

//          __N_       ___K___       
//         |    |     |       |
//       N |    |   N |       |                                 
//         |____|     |_______|                               
//   full_N     .     .       .                                                                     
//         .    .     .       .                                  
//         .    .     .       .                                        
//         ......     .........                   

//                   *Make DRAMs match big rectangles so                                 
//                      maxj doesn't complain, but only fill                              
//                      and load valid stuff                                  


//     ##################
//     # MatMult Kernel #
//     ##################


//                 Horizontal Blocking
//                 _______________     _________LDB___________     
//                |               |   |                       |    
//                |               |   |                       |    
//                |      id0      |   |                       |    
//                |__LDA__↓       |   |_______________________|    
//                |_______|_\     |  inner_N__________________|    
//                |               |   |                       |    
//                |               |   |                       |    
//                |_______________|   |_______________________|    


//                 Vertical Blocking


//                 _______________     _________LDB___________                                                                     
//                |               |   |                       |                                                                                                        
//                |               |   |                       |                                                                                                        
//                |               |   |                       |                                                                                                        
//                |  id0,1        |   |_______________________|                                       
//                |    ↳|_\       |  K|_______________________|                                       
//                |     | |       |   |                       |                                       
//                |   LDA |       |   |                       |                                                                   
//                |_____|_|_______|   |_______________________|                                              
