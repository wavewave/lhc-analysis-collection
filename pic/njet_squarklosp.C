{ 
  TFile s500("SimplifiedSUSYMN500.0MG1500.0MSQ1000.0_total_LHC8ATLAS_NoMatch_NoCut_AntiKT0.4_NoTau_Set_NJet.root"); 
  TH1F* h_s500 = (TH1F*) s500.Get("NJet");   
  h_s500 -> SetLineColor(46); 
  h_s500 -> Draw(); 
  h_s500 -> SetTitle("M_{#tilde{g}}=1500 GeV, M_{#tilde{q}}=1000 GeV");
  h_s500 -> SetXTitle("N_{jet}");
  h_s500 -> SetYTitle("number of events");

  TLatex l6(-0.2,515,"m_{N}=500"); 
  l6.SetTextColor(46); 
  l6.Draw("same"); 


  TFile u100("ADMXUDD112degenMG1500.0MQ1000.0ML50000.0MN50000.0_total_LHC8ATLAS_NoMatch_NoCut_AntiKT0.4_NoTau_Set_NJet.root");
  TH1F* h_u100 = (TH1F*) u100.Get("NJet"); 
  h_u100 -> SetLineColor(38); 
  h_u100 -> Draw("same"); 
 
  TFile s100("SimplifiedSUSYMN100.0MG1500.0MSQ1000.0_total_LHC8ATLAS_NoMatch_NoCut_AntiKT0.4_NoTau_Set_NJet.root"); 
  TH1F* h_s100 = (TH1F*) s100.Get("NJet");   
  h_s100 -> SetLineColor(42); 
  h_s100 -> Draw("same"); 


  TLatex l1(4,65,"XUDD"); 
  l1.SetTextColor(38);
  l1.Draw("same"); 


  TLatex l2(-0.2,465,"m_{N}=100"); 
  l2.SetTextColor(42);
  l2.Draw("same"); 



 
  TFile s300("SimplifiedSUSYMN300.0MG1500.0MSQ1000.0_total_LHC8ATLAS_NoMatch_NoCut_AntiKT0.4_NoTau_Set_NJet.root"); 
  TH1F* h_s300 = (TH1F*) s300.Get("NJet");   
  h_s300 -> SetLineColor(44); 
  h_s300 -> Draw("same"); 

  TLatex l4(-0.2,490,"m_{N}=300"); 
  l4.SetTextColor(44); 
  l4.Draw("same"); 





 

  
}
