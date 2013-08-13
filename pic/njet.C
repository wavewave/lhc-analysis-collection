{ 
  TFile s500("SimplifiedSUSYMN500.0MG1000.0MSQ1000.0_total_LHC8ATLAS_NoMatch_NoCut_AntiKT0.4_NoTau_Set_NJet.root"); 
  TH1F* h_s500 = (TH1F*) s500.Get("NJet");   
  h_s500 -> SetLineColor(46); 
  h_s500 -> Draw(); 
  h_s500 -> SetTitle("M_{#tilde{g}}=M_{#tilde{q}}=1000 GeV");
  h_s500 -> SetXTitle("N_{jet}");
  h_s500 -> SetYTitle("number of events");

  TLatex l5(8,100,"m_{N}=500"); 
  l5.SetTextColor(38);
  TLatex l6(4,95,"m_{N}=500"); 
  l6.SetTextColor(46); 
  l5.Draw("same"); 
  l6.Draw("same"); 


  TFile u100("ADMXUDD112degenMG1000.0MQ1000.0ML50000.0MN100.0_total_LHC8ATLAS_NoMatch_NoCut_AntiKT0.4_NoTau_Set_NJet.root");
  TH1F* h_u100 = (TH1F*) u100.Get("NJet"); 
  h_u100 -> SetLineColor(30); 
  h_u100 -> Draw("same"); 
 
  TFile s100("SimplifiedSUSYMN100.0MG1000.0MSQ1000.0_total_LHC8ATLAS_NoMatch_NoCut_AntiKT0.4_NoTau_Set_NJet.root"); 
  TH1F* h_s100 = (TH1F*) s100.Get("NJet");   
  h_s100 -> SetLineColor(42); 
  h_s100 -> Draw("same"); 

  TLatex l1(8,100,"m_{N}=100");
  l1.SetTextColor(30);  
  TLatex l2(4,65,"m_{N}=100"); 
  l2.SetTextColor(42);
  l1.Draw("same"); 
  l2.Draw("same"); 




  TFile u300("ADMXUDD112degenMG1000.0MQ1000.0ML50000.0MN300.0_total_LHC8ATLAS_NoMatch_NoCut_AntiKT0.4_NoTau_Set_NJet.root");
  TH1F* h_u300 = (TH1F*) u300.Get("NJet"); 
  h_u300 -> SetLineColor(35); 
  h_u300 -> Draw("same"); 
  h_u300 -> SetTitle("Missing ET, mneut=300 GeV, mgluino=msquark=1000 GeV");
 
  TFile s300("SimplifiedSUSYMN300.0MG1000.0MSQ1000.0_total_LHC8ATLAS_NoMatch_NoCut_AntiKT0.4_NoTau_Set_NJet.root"); 
  TH1F* h_s300 = (TH1F*) s300.Get("NJet");   
  h_s300 -> SetLineColor(44); 
  h_s300 -> Draw("same"); 

  TLatex l3(8,100,"m_{N}=300"); 
  l3.SetTextColor(35); 
  TLatex l4(4,80,"m_{N}=300"); 
  l4.SetTextColor(44); 
  l3.Draw("same"); 
  l4.Draw("same"); 





  TFile u500("ADMXUDD112degenMG1000.0MQ1000.0ML50000.0MN500.0_total_LHC8ATLAS_NoMatch_NoCut_AntiKT0.4_NoTau_Set_NJet.root");
  TH1F* h_u500 = (TH1F*) u500.Get("NJet"); 
  h_u500 -> SetLineColor(38); 
  h_u500 -> Draw("same"); 
  h_u500 -> SetTitle("Missing ET, mneut=500 GeV, mgluino=msquark=1000 GeV");
 

  
}
