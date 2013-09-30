{ 
  gStyle->SetOptStat(0);  

  TFile u100("ADMXUDD112degenMG2500.0MQ1500.0ML50000.0MN50000.0_total_LHC8ATLAS_NoMatch_NoCut_AntiKT0.4_NoTau_Set_NJet.root");
  TH1F* h_u100 = (TH1F*) u100.Get("NJet"); 
  h_u100 -> SetLineColor(38); 
  h_u100 -> Draw(); 
  h_u100 -> SetTitle("M_{#tilde{g}}=2500 GeV, M_{#tilde{q}}=1500 GeV");
  h_u100 -> SetXTitle("N_{jet}");
  h_u100 -> SetYTitle("number of events");


  TFile s500("SimplifiedSUSYMN500.0MG2500.0MSQ1500.0_total_LHC8ATLAS_NoMatch_NoCut_AntiKT0.4_NoTau_Set_NJet.root"); 
  TH1F* h_s500 = (TH1F*) s500.Get("NJet");   
  h_s500 -> SetLineColor(46); 
  h_s500 -> Draw("same"); 

  TLatex l6(-0.2,14,"m_{N}=500"); 
  l6.SetTextColor(46); 
  l6.Draw("same"); 


 
  TFile s100("SimplifiedSUSYMN100.0MG2500.0MSQ1500.0_total_LHC8ATLAS_NoMatch_NoCut_AntiKT0.4_NoTau_Set_NJet.root"); 
  TH1F* h_s100 = (TH1F*) s100.Get("NJet");   
  h_s100 -> SetLineColor(42); 
  h_s100 -> Draw("same"); 


  TLatex l1(4,10,"UDD"); 
  l1.SetTextColor(38);
  l1.Draw("same"); 


  TLatex l2(-0.2,11,"m_{N}=100"); 
  l2.SetTextColor(42);
  l2.Draw("same"); 



 
  TFile s300("SimplifiedSUSYMN300.0MG2500.0MSQ1500.0_total_LHC8ATLAS_NoMatch_NoCut_AntiKT0.4_NoTau_Set_NJet.root"); 
  TH1F* h_s300 = (TH1F*) s300.Get("NJet");   
  h_s300 -> SetLineColor(44); 
  h_s300 -> Draw("same"); 

  TLatex l4(-0.2,12.5,"m_{N}=300"); 
  l4.SetTextColor(44); 
  l4.Draw("same"); 


  gPad->Update();
  Double_t x1 = gPad->GetX1(); 
  Double_t x2 = gPad->GetX2();
  Double_t y1 = gPad->GetY1(); 
  Double_t y2 = gPad->GetY2(); 
  Double_t xr = x2-x1; 
  Double_t yr = y2-y1; 

  TBox b1(xr*0.7+x1,yr*0.82+y1,xr*0.72+x1,yr*0.85+y1); 
  b1.SetFillColor(42); 
  b1.Draw("same"); 

  TBox b2(xr*0.73+x1,yr*0.82+y1,xr*0.75+x1,yr*0.85+y1); 
  b2.SetFillColor(44); 
  b2.Draw("same"); 

  TBox b3(xr*0.76+x1,yr*0.82+y1,xr*0.78+x1,yr*0.85+y1); 
  b3.SetFillColor(46); 
  b3.Draw("same"); 
 
  TLatex lsim(xr*0.8+x1,yr*0.82+y1,"Sim0"); 
  lsim.SetTextColor(46); 
  lsim.Draw("same"); 

  TBox b4(xr*0.7+x1,yr*0.75+y1,xr*0.78+x1,yr*0.78+y1); 
  b4.SetFillColor(38); 
  b4.Draw("same"); 
  

  TLatex lqld(xr*0.8+x1,yr*0.75+y1,"UDD"); 
  lqld.SetTextColor(38); 
  lqld.Draw("same"); 

   



 

  
}
