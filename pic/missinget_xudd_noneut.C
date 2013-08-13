{ 
  
  TFile l("ADMXUDD112degenMG1500.0MQ1000.0ML50000.0MN50000.0_total_LHC8ATLAS_NoMatch_NoCut_AntiKT0.4_NoTau_Set_MET.root");
  TH1F* h_l = (TH1F*) l.Get("2jet"); 
  h_l -> SetLineColor(30); 
  h_l -> SetTitle("M_{#tilde{g}}=1500 GeV, M_{#tilde{q}}=1000 GeV");
  h_l -> SetXTitle("E_{T}^{miss}");
  h_l -> SetYTitle("Number of Events");  
  h_l -> Draw(); 




  TFile s500("SimplifiedSUSYMN500.0MG1500.0MSQ1000.0_total_LHC8ATLAS_NoMatch_NoCut_AntiKT0.4_NoTau_Set_MET.root"); 
  TH1F* h_s500 = (TH1F*) s500.Get("2jet");   
  h_s500 -> SetLineColor(46); 
  h_s500 -> Draw("same"); 

  TFile s100("SimplifiedSUSYMN100.0MG1500.0MSQ1000.0_total_LHC8ATLAS_NoMatch_NoCut_AntiKT0.4_NoTau_Set_MET.root"); 
  TH1F* h_s100 = (TH1F*) s100.Get("2jet");   
  h_s100 -> SetLineColor(42); 
  h_s100 -> Draw("same"); 



  TLatex l1(150,30,"XUDD");
  l1.SetTextColor(30);  
  TLatex l2(600,30,"m_{N}=100"); 
  l2.SetTextColor(42);
  l1.Draw("same"); 
  l2.Draw("same"); 
  
  

 
  TFile s300("SimplifiedSUSYMN300.0MG1500.0MSQ1000.0_total_LHC8ATLAS_NoMatch_NoCut_AntiKT0.4_NoTau_Set_MET.root"); 
  TH1F* h_s300 = (TH1F*) s300.Get("2jet");   
  h_s300 -> SetLineColor(44); 
  h_s300 -> Draw("same"); 

  TLatex l4(550,30,"m_{N}=300"); 
  l4.SetTextColor(44); 
  l4.Draw("same"); 



 

  TLatex l6(500,30,"m_{N}=500"); 
  l6.SetTextColor(46); 
  l6.Draw("same"); 
   
  

}
